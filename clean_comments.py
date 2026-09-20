#!/usr/bin/env python3
"""
Prépare le dépôt pour la release publique (Zenodo).

Deux opérations :

1. Dans les fichiers .R : supprime les blocs #TODO / #NOTE / #INFO / #WARN.
   Un bloc commence par une ligne portant un marqueur et continue tant que les
   lignes suivantes sont des commentaires (#).

2. Dans README.md : tronque le fichier juste avant la section
   "# Release on Zenodo" (celle-ci incluse). Tout ce qui suit est interne au
   projet (procédure de release, workflow GitHub en italien) et n'a pas à être
   publié. Les sections antérieures — source des données, installation, mode
   d'emploi — sont conservées.

Usage :
    python clean_comments.py             # applique les modifications
    python clean_comments.py --dry-run   # affiche ce qui serait supprimé
    python clean_comments.py --log       # écrit clean_comments_log.md
"""

import re
import sys
from pathlib import Path

MARKERS = re.compile(r'^\s*#\s*(TODO|NOTE|INFO|WARN)\b', re.IGNORECASE)
COMMENT = re.compile(r'^\s*#')

# Titre de la première section interne du README : ce titre et tout ce qui le
# suit sont retirés de la version publiée.
README_CUT_HEADING = re.compile(r'^#\s+Release on Zenodo\s*$', re.IGNORECASE)


def clean_file(path: Path, dry_run: bool = False) -> tuple[int, list]:
    """Retourne (nb_lignes_supprimées, liste de blocs supprimés)."""
    lines = path.read_text(encoding='utf-8').splitlines(keepends=True)
    result = []
    removed = 0
    blocks = []  # liste de (ligne_debut, contenu_bloc)
    i = 0
    while i < len(lines):
        if MARKERS.match(lines[i]):
            j = i
            while j < len(lines) and COMMENT.match(lines[j]):
                j += 1
            bloc = lines[i:j]
            blocks.append((i + 1, ''.join(bloc)))  # numéro de ligne 1-based
            removed += j - i
            i = j
        else:
            result.append(lines[i])
            i += 1

    if not dry_run:
        path.write_text(''.join(result), encoding='utf-8')

    return removed, blocks


def clean_readme(path: Path, dry_run: bool = False) -> tuple[int, str]:
    """Tronque le README juste avant '# Release on Zenodo'.

    Retourne (nb_lignes_supprimées, contenu supprimé). Si le titre n'est pas
    trouvé, ne touche à rien et retourne (0, '') — le README a peut-être déjà
    été nettoyé, ou la section a été renommée.
    """
    if not path.exists():
        return 0, ''

    lines = path.read_text(encoding='utf-8').splitlines(keepends=True)
    cut = None
    for i, line in enumerate(lines):
        if README_CUT_HEADING.match(line):
            cut = i
            break

    if cut is None:
        return 0, ''

    kept = lines[:cut]
    dropped = lines[cut:]

    # évite de laisser une pile de lignes vides en fin de fichier
    while kept and kept[-1].strip() == '':
        kept.pop()
    if kept:
        kept.append('\n')

    if not dry_run:
        path.write_text(''.join(kept), encoding='utf-8')

    return len(dropped), ''.join(dropped)


def main():
    dry_run = '--dry-run' in sys.argv
    log = '--log' in sys.argv
    root = Path('.')
    label = '[DRY RUN] ' if dry_run else ''

    total_removed = 0
    log_lines = ['# Blocs supprimés — clean_comments.py\n']

    # --- 1. scripts R ------------------------------------------------------
    for r_file in sorted(root.rglob('*.r')):
        if 'renv' in r_file.parts:
            continue
        removed, blocks = clean_file(r_file, dry_run=dry_run)
        if removed:
            print(f"{label}{r_file}: {removed} ligne(s) supprimée(s)")
            total_removed += removed

            if log and blocks:
                log_lines.append(f'\n## `{r_file}`\n')
                for lineno, bloc in blocks:
                    log_lines.append(f'\n**Ligne {lineno}**\n```r\n{bloc.rstrip()}\n```\n')

    # --- 2. README.md ------------------------------------------------------
    readme = root / 'README.md'
    removed, dropped = clean_readme(readme, dry_run=dry_run)
    if removed:
        print(
            f"{label}{readme}: {removed} ligne(s) supprimée(s) "
            f"(à partir de « # Release on Zenodo » incluse)"
        )
        total_removed += removed
        if log:
            log_lines.append('\n## `README.md`\n')
            log_lines.append(
                '\nSection interne retirée (à partir de « # Release on Zenodo ») :\n'
                f'```markdown\n{dropped.rstrip()}\n```\n'
            )
    elif readme.exists():
        print(
            f"{label}{readme}: titre « # Release on Zenodo » introuvable, "
            "fichier laissé tel quel"
        )

    print(f"\nTotal : {total_removed} ligne(s) "
          f"{'qui seraient supprimées' if dry_run else 'supprimées'}")

    if log:
        log_path = Path('clean_comments_log.md')
        log_path.write_text(''.join(log_lines), encoding='utf-8')
        print(f"Log écrit dans {log_path}")


if __name__ == '__main__':
    main()

#!/usr/bin/env python3
"""
Supprime les blocs #TODO / #NOTE / #INFO / #WARN des fichiers .R
Un bloc commence par une ligne avec un marqueur et continue
tant que les lignes suivantes sont des commentaires (#).
"""

import re
import sys
from pathlib import Path

MARKERS = re.compile(r'^\s*#\s*(TODO|NOTE|INFO|WARN)\b', re.IGNORECASE)
COMMENT = re.compile(r'^\s*#')

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

def main():
    dry_run = '--dry-run' in sys.argv
    log = '--log' in sys.argv
    root = Path('.')

    total_removed = 0
    log_lines = ['# Blocs supprimés — clean_comments.py\n']

    for r_file in sorted(root.rglob('*.r')):
        if 'renv' in r_file.parts:
            continue
        removed, blocks = clean_file(r_file, dry_run=dry_run)
        if removed:
            label = '[DRY RUN] ' if dry_run else ''
            print(f"{label}{r_file}: {removed} ligne(s) supprimée(s)")
            total_removed += removed

            if log and blocks:
                log_lines.append(f'\n## `{r_file}`\n')
                for lineno, bloc in blocks:
                    log_lines.append(f'\n**Ligne {lineno}**\n```r\n{bloc.rstrip()}\n```\n')

    print(f"\nTotal : {total_removed} ligne(s) {'qui seraient supprimées' if dry_run else 'supprimées'}")

    if log:
        log_path = Path('clean_comments_log.md')
        log_path.write_text(''.join(log_lines), encoding='utf-8')
        print(f"Log écrit dans {log_path}")

if __name__ == '__main__':
    main()

#!/usr/bin/env python3

"""
Prints the size of the distribution files (wheels, sdist) given as arguments
and, for the wheels, the size of their largest members (uncompressed and
compressed). With --summary FILE, a markdown table of the file sizes is also
appended to FILE (e.g. $GITHUB_STEP_SUMMARY).
PyPI refuses files larger than 100 MB (unless a larger limit is granted).
"""

import argparse
import os
import zipfile

PYPI_LIMIT = 100 * 1024 ** 2


def human(size):
    """Size in a human readable form"""
    for unit in ('B', 'kB', 'MB', 'GB'):
        if size < 1024 or unit == 'GB':
            return f'{size:.1f} {unit}' if unit != 'B' else f'{size} B'
        size /= 1024
    return None


def main():
    """Main function"""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('files', nargs='+', help='distribution files')
    parser.add_argument('--summary', help='markdown file to which the table is appended')
    parser.add_argument('--members', type=int, default=10,
                        help='number of wheel members to list (default: 10)')
    args = parser.parse_args()

    rows = []
    for path in args.files:
        size = os.path.getsize(path)
        name = os.path.basename(path)
        warning = ' (LARGER THAN THE PYPI LIMIT)' if size > PYPI_LIMIT else ''
        print(f'{name}: {human(size)}{warning}')
        uncompressed = None
        if path.endswith('.whl'):
            with zipfile.ZipFile(path) as wheel:
                infos = wheel.infolist()
            uncompressed = sum(info.file_size for info in infos)
            print(f'  {len(infos)} files, {human(uncompressed)} uncompressed; largest:')
            for info in sorted(infos, key=lambda info: info.file_size,
                               reverse=True)[:args.members]:
                print(f'  {human(info.file_size):>10} ({human(info.compress_size):>10} compressed)'
                      f'  {info.filename}')
        rows.append((name, size, uncompressed, warning))

    if args.summary:
        with open(args.summary, 'a', encoding='utf-8') as summary:
            summary.write('| File | Size | Uncompressed |\n|---|---:|---:|\n')
            for name, size, uncompressed, warning in rows:
                summary.write(f'| {name} | {human(size)}{warning} | '
                              f'{"" if uncompressed is None else human(uncompressed)} |\n')
            summary.write('\n')


if __name__ == '__main__':
    main()

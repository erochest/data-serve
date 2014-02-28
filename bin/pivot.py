#!/usr/bin/env python


from __future__ import unicode_literals, print_function

from collections import namedtuple
import csv
import itertools
import sys


InternetUser = namedtuple(
    'InternetUser',
    'id country country_code ser_code ser_name year value'.split(),
    )

YEAR_FIELDS = [ str(yr) for yr in range(1960, 2014) ]


def expand_row(input_row):
    row = InternetUser(
        None, 
        input_row['Country Name'],
        input_row['Country Code'],
        input_row['Indicator Code'],
        input_row['Indicator Name'],
        None,
        None,
        )

    for yr in YEAR_FIELDS:
        yield row._replace(year=yr, value=input_row[yr])


def main():
    reader = csv.DictReader(sys.stdin)
    writer = csv.writer(sys.stdout)

    rows = itertools.chain.from_iterable( expand_row(r) for r in reader )
    with_n = itertools.izip(itertools.count(0), rows)
    rows_n = itertools.imap(lambda (n, r): r._replace(id=n), with_n)

    writer.writerow(InternetUser._fields)
    writer.writerows(rows_n)


if __name__ == '__main__':
    main()

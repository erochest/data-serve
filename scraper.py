#!/usr/bin/env python


from __future__ import unicode_literals, print_function

import csv
try:
    import urlparse
except ImportError:
    import urllib.parse as urlparse

import cssselect
import lxml.cssselect
import lxml.html
import lxml.etree
import requests


BASEURL = 'http://ancient-shore-4835.herokuapp.com/'
COUNTRY = 'data.country'
PAGE    = 'data.page'
OUTPUT  = 'internet-users-output.csv'
FIELDS  = ('Country', 'Country Code', 'Year', 'Value')


def get(url, params=None):
    """A basic utility to get and parse a web page."""
    req = requests.get(url, params=params)
    doc = lxml.html.fromstring(req.text)
    return doc


def get_action_url(base_url, doc):
    """This returns the destination URL for getting the tables."""
    for form in doc.cssselect('form'):
        return urlparse.urljoin(base_url, form.get('action'))
    return None


def get_countries(doc):
    """Takes the document and returns (country, country_value)."""
    countries = []
    for select in doc.cssselect('form select'):
        if select.get('name') == COUNTRY:
            for option in select.cssselect('option'):
                countries.append((option.text, option.get('value')))
    return countries


def get_country_data(url, country_code, page=0):
    """Page through the data for one country."""
    country_data = []
    doc = get(url, {PAGE: page, COUNTRY: country_code})

    # Get the data for the current page
    for table_row in doc.cssselect('table tbody tr'):
        country_data.append(
            tuple(td.text for td in table_row.cssselect('td'))
            )

    # If this page has data, see if the next does too.
    if country_data:
        country_data += get_country_data(url, country_code, page + 1)

    return country_data


def get_internet_users(base_url):
    """This controls the process."""
    doc = get(base_url)
    action_url = get_action_url(base_url, doc)
    internet_users = []
    for (country, country_code) in get_countries(doc):
        print(country)
        internet_users += get_country_data(action_url, country_code)
    print('Done')
    return internet_users


def main(base_url=BASEURL, output=OUTPUT, fields=FIELDS):
    """Pull in the input from the web and write it to a file."""
    with open(output, 'w') as file_out:
        writer = csv.writer(file_out)
        writer.writerow(fields)
        writer.writerows(get_internet_users(base_url))


if __name__ == '__main__':
    main()

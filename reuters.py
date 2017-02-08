
"""Searches Reuters and dumps out articles."""


from __future__ import print_function

import requests
# Since this example parses loosely structured data, we'll use BeautifulSoup.
from bs4 import BeautifulSoup

# Some of the weird stuff in this file is to work under both Python 2.7 and
# Python 3.6.
try:
    from urlparse import urljoin
except ImportError:
    from urllib.parse import urljoin

import csv
import json
import sys


# A term to search for
SEARCH = 'iphone'

# The URL for loading the data as a snippet of JavaScript. Don't pay attention
# to what the URL says it does. It lies.
SEARCH_URL = 'http://www.reuters.com/assets/searchArticleLoadMoreJson'


def get(url, params):
    """Get and parse a URL."""
    response = requests.get(url, params=params)
    return BeautifulSoup(response.text, 'html.parser')


def search(term, url=SEARCH_URL):
    """Search for a term and return a list of result IDs and URLs."""
    results = []

    # Currently, I have limited this to get only the first five pages of
    # results.
    for page in range(1, 5):

        # I got these values by looking in the "Network" tab of browser's
        # Developer Tools. When I clicked to load more items on the page, I
        # looked at the URL it was requested, and I've replicated that here.
        params = {
            'blob': term,
            'bigOrSmall': 'big',
            'articleWithBlog': 'true',
            'sortBy': '',
            'dateRange': '',
            'numResultsToShow': 10,
            'pn': page,
            'callback': 'hi',
        }
        response = requests.get(url, params=params)
        news = parse_news_data(response.text)
        hits = json.loads(news)
        if hits:
            # Save the IDs and full URLs for each article.
            for hit in hits:
                results.append((hit['id'], urljoin(url, hit['href'])))
        else:
            # Otherwise, if there aren't any hits on a page, we can stop asking
            # for more.
            break

    return results


# The results are a snippet of JavaScript that contains the data as part of a
# JavaScript object (i.e., not real JSON). We have to parse through the
# JavaScript and look for "news:". Then we accumulate everything until we get
# to the end of that data structure. But inside that data, we also have to wrap
# all of the properties of the object in double quotes. Sad!
def parse_news_data(content):
    """Parse the content into JSON data."""
    news = ''
    # This tracks whether we're in the structure to save or not.
    in_news = False

    for line in content.splitlines():
        line = line.strip()

        # The beginning of the structure.
        if not in_news and line.startswith('news: '):
            in_news = 'in'
            news = '['

        # The end.
        elif in_news and line.startswith(']'):
            news += ']'
            break

        # Everything in between.
        elif in_news:
            pair = line.split(':', 1)
            if len(pair) == 2:
                news += '"{}": {}'.format(*pair)
            else:
                news += line

    return news


def normalize(content):
    """Normalizes whitespace."""
    return ' '.join(content.split())


def get_article(url):
    """Return the article title and text found at URL."""
    page = get(url, None)
    title = normalize(page.find('h1', class_='article-headline').get_text())
    text = page.find(id='article-text').get_text()
    return (title, text)


def main():
    """Entry function."""
    # Open a file.
    with open('reuters.csv', 'w') as file_out:
        # Wrap it in something to format data sent to it as CSV.
        writer = csv.writer(file_out)
        # Output a title.
        writer.writerow(['id', 'url', 'title', 'text'])
        # For each hit in the search results...
        for ident, url in search('iphone'):
            # Get the article.
            title, text = get_article(url)
            if sys.version_info[0] == 3:
                # Python 3 handles this just fine.
                data = (ident, url, title, text)
            else:
                # But Python 2 requires you to convert it to a string (not
                # Unicode).
                data = (
                    ident.encode('utf8'),
                    url.encode('utf8'),
                    title.encode('utf8'),
                    text.encode('utf8'),
                    )
            writer.writerow(data)


if __name__ == '__main__':
    main()

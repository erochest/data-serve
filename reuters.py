
"""Searches Reuters and dumps out articles."""


from __future__ import print_function

import requests
from bs4 import BeautifulSoup
try:
    from urlparse import urljoin
except ImportError:
    from urllib.parse import urljoin
import csv
import json
import sys


SEARCH = 'iphone'
SEARCH_URL = 'http://www.reuters.com/assets/searchArticleLoadMoreJson'


def get(url, params):
    """Get and parse a URL."""
    response = requests.get(url, params=params)
    return BeautifulSoup(response.text, 'html.parser')


def search(term, url=SEARCH_URL):
    """Search for a term and return a list of result IDs and URLs."""
    results = []
    for page in range(1, 5):
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
        for hit in hits:
            results.append((hit['id'], urljoin(url, hit['href'])))
    return results


def parse_news_data(content):
    """Parse the content into JSON data."""
    news = ''
    in_news = False
    for line in content.splitlines():
        line = line.strip()
        if not in_news and line.startswith('news: '):
            in_news = 'in'
            news = '['
        elif in_news and line.startswith(']'):
            news += ']'
            break
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
    """Return the article found at URL."""
    page = get(url, None)
    title = normalize(page.find('h1', class_='article-headline').get_text())
    text = page.find(id='article-text').get_text()
    return (title, text)


def main():
    """Entry function."""
    with open('reuters.csv', 'w') as file_out:
        writer = csv.writer(file_out)
        writer.writerow(['id', 'url', 'title', 'text'])
        for ident, url in search('iphone'):
            title, text = get_article(url)
            if sys.version_info[0] == 3:
                data = (ident, url, title, text)
            else:
                data = (
                    ident.encode('utf8'),
                    url.encode('utf8'),
                    title.encode('utf8'),
                    text.encode('utf8'),
                    )
            writer.writerow(data)


if __name__ == '__main__':
    main()

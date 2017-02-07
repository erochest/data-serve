% Introduction to Screen Scraping
% Eric Rochester, Scholars' Lab
% bit.ly/erochest-screen-scraping

# About the Scholars' Lab

# Introduction to Screen Scraping

## For Today

* Overview
* Process
* A few tools
* A quick example

# What is It?

# Caveats

## Legal Issues

Read the fine print.

Wikipedia has a nice overview on the [Web Scraping article][legal].

## Not Always Possible

# Levels of Accessibility

## Downloadable

* Comma-separated-values
* JSON
* Excel

## HTML

## Inaccessible

<p class="fragment">Looking at you, Flash</p>

<p class="fragment">And you, PDF</p>

# Tools

## Python

[http://www.python.org/][python]

## Requests

[http://docs.python-requests.org/en/latest/][requests]

## lxml or BeautifulSoup

* [http://lxml.de/index.html][lxml]
* [http://www.crummy.com/software/BeautifulSoup/][soup]

## Or Almost Any Other Programming Language

## Your Browser!

[Chrome][chrome] and [Firefox][firefox] both come with lots of tools.

# The Example Today

## Internet Users

[http://ancient-shore-4835.herokuapp.com/][eg]

# How to Approach the Problem

## Think about How You Get to the Data

## Duplicate That!

## Look for One Entry Point

## Branch off

# Let's Explore

## Main Page

## Form to Subdivide the Data

## Data in Table

## Paginated

# I Want to Go There

## Not a Tutorial on Python

## Download All the Things!

```python
def get(url, params=None):
    """A basic utility to get and parse a web page."""
    req = requests.get(url, params=params)
    doc = lxml.html.fromstring(req.text)
    return doc
```

## World Traveller

```python
def get_countries(base_url, doc):
    """Takes the document and returns (country, country_value)."""
    for select in doc.cssselect('form select'):
        if select.get('name') == COUNTRY:
            for option in select.cssselect('option'):
                yield (option.text, option.get('value'))
```

## On the Table

```python
def get_country_data(url, country_code, page=0):
    """Page through the data for one country."""
    doc = get(url, {PAGE: page, COUNTRY: country_code})

    # Get the data for the current page, counting it as we go.
    n = 0
    for table_row in doc.cssselect('table tbody tr'):
        n += 1
        yield tuple( td.text for td in table_row.cssselect('td') )

    # If this page has data, see if the next does too.
    if n > 0:
        for row in get_country_data(url, country_code, page + 1):
            yield row
```

## But That's not All

See [the full source][source].

# Where to Go from Here

## This Presentation (with Links)

[http://www.ericrochester.com/data-serve/][this]

## More Links

* [Python][python]
* [Learn Python the Hard Way][hardway]
* [Python Package Index][pypi] (PyPI, the collection of Python libraries)
* [Requests][requests] library for getting web pages
* [lxml][lxml] library for digging into web pages
* [Mozilla Developer Network][mdn] information, references, and tutorials about the
  technologies that power the web.

# Questions?

[legal]: http://en.wikipedia.org/wiki/Web_scraping#Legal_issues
[python]: http://www.python.org/
[requests]: http://docs.python-requests.org/en/latest/
[lxml]: http://lxml.de/index.html
[soup]: http://www.crummy.com/software/BeautifulSoup/
[cssselect]: http://pythonhosted.org/cssselect/
[chrome]: http://www.google.com/chrome/
[firefox]: http://www.mozilla.org/en-US/firefox/new/
[eg]: http://ancient-shore-4835.herokuapp.com/
[this]: http://www.ericrochester.com/data-serve/
[source]: https://github.com/erochest/data-serve/blob/master/scraper.py
[hardway]: http://learnpythonthehardway.org/
[pypi]: https://pypi.python.org/pypi
[mdn]: https://developer.mozilla.org/

#!/usr/bin/env python

"""
Convert HDL API from http://www.nand2tetris.org/software/HDL%20Survival%20Guide.html
to snippets for emacs.
"""

import sys
from bs4 import BeautifulSoup

if sys.version_info[0] == 3:
    import urllib.request as urllib
    from urllib.error import HTTPError
else:
    import urllib2 as urllib
    from urllib2 import HTTPError


class HdlApi:

    def __init__(self, url):
        self.soup = None

        try:
            html = urllib.urlopen(url).read()
            self.soup = BeautifulSoup(html)

        except HTTPError as e:
            print(e)

    def get_api(self):
        try:
            api = self.soup.findAll('pre')[-1].get_text()
            for i in api.split('\n'):
                if len(i):
                    write_snippet(i.strip())
        except:
            print "Failed to get api"


def make_snippet(line):
    ints = iter(range(1, line.count(' ') + 1))
    res = "".join(c if c != ' ' else '$' + str(next(ints)) for c in line)
    return res + "$0"


def write_snippet(line):
    name = line[:line.index('(')]
    snippet = make_snippet(line)
    res = "\n".join(["# -*- mode: snippet -*-",
                     "# name: " + line,
                     "# key: " + name,
                     "# --",
                     snippet])
    with open(name, "w") as f:
        f.write(res)


if __name__ == '__main__':
    import os
    os.mkdir("nand-hdl-mode")
    os.chdir("nand-hdl-mode")

    h = HdlApi("http://www.nand2tetris.org/software/HDL%20Survival%20Guide.html")
    h.get_api()

#!/usr/bin/python

import urllib
import urllib2
import string
import sys
import textwrap
from bs4 import BeautifulSoup

urls = ["http://en.wikisource.org/wiki/The_Narrative_of_Arthur_Gordon_Pym/Chapter_"+str(i) for i in xrange(1,26)]
urls = ["http://en.wikisource.org/wiki/The_Narrative_of_Arthur_Gordon_Pym/Preface"] + urls
urls = urls + ["http://en.wikisource.org/wiki/The_Narrative_of_Arthur_Gordon_Pym/Note"]

user_agent = 'Mozilla/5 (Solaris 10) Gecko'
headers = { 'User-Agent' : user_agent }

for url in urls:
    request = urllib2.Request(url, headers=headers)
    response = urllib2.urlopen(request)
    page = response.read()
    s = BeautifulSoup(page)
    cont = s.find(id="mw-content-text")
    print "\\section{"+cont.find(id="header_section_text").string+"}"
    for c in cont.find_all("p", recursive=False):
        c = textwrap.fill(str(c), 80, break_long_words=False, break_on_hyphens=False)
        print c,"\n" # removes <i> and stuff!
    



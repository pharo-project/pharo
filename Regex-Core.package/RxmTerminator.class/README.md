-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
Instances of this class are used to terminate matcher's chains. When a match reaches this (an instance receives #matchAgainst: message), the match is considered to succeed. Instances also support building protocol of RxmLinks, with some restrictions.
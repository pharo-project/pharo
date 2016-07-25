-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
A marker is used to remember positions of match of certain points of a regular expression. The marker receives an identifying key from the Matcher and uses that key to report positions of successful matches to the Matcher.

Instance variables:
	index	<Object> Something that makes sense for the Matcher. Received from the latter during initalization and later passed to it to identify the receiver.
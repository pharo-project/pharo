testing
testPlusHandling
	"While percent decoding, a + is translated as a space only in the context of 
	application/x-www-form-urlencoded get/post requests:
	http://en.wikipedia.org/wiki/Percent-encoding#The_application.2Fx-www-form-urlencoded_type
	ZnUrl interprets its query part as key value pairs where this translation is applicable,
	even though strictly speaking + (and =, &) are plain unreserved characters in the query part"
	
	"$+ is not special in the path part of the URL and it remains itself"
	self 
		assert: 'http://localhost/foo+bar' asZnUrl firstPathSegment 
		equals: 'foo+bar'.
	self 
		assert: 'http://localhost/foo+bar' asZnUrl printString 
		equals: 'http://localhost/foo+bar'.
	"$+ gets decoded to space in the interpreted query part of the URL, 
	and becomes an encoded space if needed"
	self 
		assert: ('http://localhost/test?q=foo+bar' asZnUrl queryAt: #q)
		equals: 'foo bar'.
	self 
		assert: 'http://localhost/test?q=foo+bar' asZnUrl printString
		equals: 'http://localhost/test?q=foo%20bar'.
	"to pass $+ as $+ in a query, it has to be encoded"
	self 
		assert: 'http://localhost/test?q=foo%2Bbar' asZnUrl printString
		equals: 'http://localhost/test?q=foo%2Bbar'
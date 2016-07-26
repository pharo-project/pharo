-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
A matcher is built of a number of links interconnected into some intricate structure. Regardless of fancy stuff, any link (except for the terminator) has the next one. Any link can match against a stream of characters, recursively propagating the match to the next link. Any link supports a number of matcher-building messages. This superclass does all of the above. 

The class is not necessarily abstract. It may double as an empty string matcher: it recursively propagates the match to the next link, thus always matching nothing successfully.

Principal method:
	matchAgainst: aMatcher
		Any subclass will reimplement this to test the state of the matcher, most
		probably reading one or more characters from the matcher's stream, and
		either decide it has matched and answer true, leaving matcher stream
		positioned at the end of match, or answer false and restore the matcher
		stream position to whatever it was before the matching attempt.

Instance variables:
	next		<RxmLink | RxmTerminator> The next link in the structure.
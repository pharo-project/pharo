housekeeping
removeAllLineFeedsQuietly
	"Smalltalk removeAllLineFeedsQuietly"
	"Scan all methods for source code with lineFeeds.
	Replaces all occurrences of <CR><LF> or <LF> by <CR>.
	Answer a Dictionary keyed by author name containing sets of affected method names,
	as well as (at the key 'OK') a list of methods that still contain LF characters inside literal strings or characters."
	^self removeAllLineFeedsQuietlyCalling: [].
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
This is a recursive regex matcher. Not strikingly efficient, but simple. Also, keeps track of matched subexpressions.  The life cycle goes as follows:

1. Initialization. Accepts a syntax tree (presumably produced by RxParser) and compiles it into a matcher built of other classes in this category.

2. Matching. Accepts a stream or a string and returns a boolean indicating whether the whole stream or its prefix -- depending on the message sent -- matches the regex.

3. Subexpression query. After a successful match, and before any other match, the matcher may be queried about the range of specific stream (string) positions that matched to certain parenthesized subexpressions of the original expression.

Any number of queries may follow a successful match, and any number or matches may follow a successful initialization.

Note that `matcher' is actually a sort of a misnomer. The actual matcher is a web of Rxm* instances built by RxMatcher during initialization. RxMatcher is just the interface facade of this network.  It is also a builder of it, and also provides a stream-like protocol to easily access the stream being matched.

Instance variables:
	matcher				<RxmLink> The entry point into the actual matcher.
	stream				<Stream> The stream currently being matched against.
	markerPositions		<Array of: Integer> Positions of markers' matches.
	markerCount		<Integer> Number of markers.
	lastResult 			<Boolean> Whether the latest match attempt succeeded or not.
	lastChar			<Character | nil> character last seen in the matcher stream
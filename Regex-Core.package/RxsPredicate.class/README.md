-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
This represents a character that satisfies a certain predicate.

Instance Variables:

	predicate	<BlockClosure>	A one-argument block. If it evaluates to the value defined by <negated> when it is passed a character, the predicate is considered to match.
	negation	<BlockClosure>	A one-argument block that is a negation of <predicate>.
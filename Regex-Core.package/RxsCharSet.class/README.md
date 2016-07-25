-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
A character set corresponds to a [...] construct in the regular expression.

Instance variables:
	elements	<OrderedCollection> An element can be one of: RxsCharacter, RxsRange, or RxsPredicate.
	negated		<Boolean>
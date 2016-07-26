-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
I am a parser created to parse the insides of a character set ([...]) construct. I create and answer a collection of "elements", each being an instance of one of: RxsCharacter, RxsRange, or RxsPredicate.

Instance Variables:

	source	<Stream>	open on whatever is inside the square brackets we have to parse.
	lookahead	<Character>	The current lookahead character
	elements	<Collection of: <RxsCharacter|RxsRange|RxsPredicate>> Parsing result
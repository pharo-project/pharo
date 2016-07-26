RBScanner is a stream that returns a sequence of token from the string that it is created on. The tokens know where they came from in the source code and which comments were attached to them.

Instance Variables:
	buffer	<PositionableStream>	Accumulates the text for the current token.
	characterType	<ByteSymbol>	The type of the next character. (e.g. #alphabetic, etc.)
	classificationTable	<Array of: Symbol>	Mapping from Character values to their characterType.
	comments	<Collection of: Interval>	Source intervals of scanned comments that must be attached to the next token.
	currentCharacter	<Character>	The character currently being processed.
	errorBlock	<BlockClosure>	The block to execute on lexical errors.
	extendedLiterals	<Boolean>	True if IBM-type literals are allowed. In VW, this is false.
	nameSpaceCharacter	<Character>	The character used to separate namespaces.
	numberType	<ByteSymbol>	The method to perform: to scan a number. 
	separatorsInLiterals	<Boolean>	True if separators are allowed within literals.
	stream	<PositionableStream>	Contains the text to be scanned.
	tokenStart	<Integer>	The source position of the beginning of the current token

Class Instance Variables:
	classificationTable	<Array>		the default classification table for all characters

Shared Variables:
	PatternVariableCharacter	<Character>	the character that starts a pattern node
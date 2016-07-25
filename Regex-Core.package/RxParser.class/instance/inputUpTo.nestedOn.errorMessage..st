private
inputUpTo: aCharacter nestedOn: anotherCharacter errorMessage: aString 
	"Accumulate input stream until <aCharacter> is encountered
	and answer the accumulated chars as String, not including
	<aCharacter>. Signal error if end of stream is encountered,
	passing <aString> as the error description."

	| accumulator nestLevel |
	accumulator := WriteStream on: (String new: 20).
	nestLevel := 0.
	[lookahead ~= aCharacter or: [nestLevel > 0]] whileTrue: 
			[#epsilon = lookahead ifTrue: [self signalParseError: aString].
			accumulator nextPut: lookahead.
			lookahead = anotherCharacter ifTrue: [nestLevel := nestLevel + 1].
			lookahead = aCharacter ifTrue: [nestLevel := nestLevel - 1].
			self next].
	^accumulator contents
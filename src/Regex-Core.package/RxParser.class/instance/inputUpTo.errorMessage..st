private
inputUpTo: aCharacter errorMessage: aString
	"Accumulate input stream until <aCharacter> is encountered
	and answer the accumulated chars as String, not including
	<aCharacter>. Signal error if end of stream is encountered,
	passing <aString> as the error description."

	| accumulator |
	accumulator := WriteStream on: (String new: 20).
	[lookahead ~= aCharacter and: [lookahead ~= #epsilon]]
		whileTrue: [
			accumulator nextPut: lookahead.
			self next].
	lookahead = #epsilon
		ifTrue: [ self signalParseError: aString ].
	^accumulator contents
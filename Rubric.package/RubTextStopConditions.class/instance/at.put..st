accessing
at: anInteger put: aSymbolOrNil
	"Set the special action associated with a character of code anInteger, or nil if none.
	The character code should be betxween 1 and 256 (presumably codePoint + 1).
	It can eventually be 257 for endOfRun action, or 258 for crossedX action for backward compatibility with historical squeak versions."
	
	anInteger = 257
		ifTrue: [
			self endOfRun: aSymbolOrNil..
			^aSymbolOrNil].
	anInteger = 258
		ifTrue: [
			self crossedX: aSymbolOrNil..
			^aSymbolOrNil].
	^stops at: anInteger put: aSymbolOrNil
scanning
justifiedPadFor: spaceIndex 
	"Compute the width of pad for a given space in a line of justified text."

	| pad |
	internalSpaces = 0 ifTrue: [^0].
	pad := paddingWidth // internalSpaces.
	spaceIndex <= (paddingWidth \\ internalSpaces)
		ifTrue: [^pad + 1]
		ifFalse: [^pad]
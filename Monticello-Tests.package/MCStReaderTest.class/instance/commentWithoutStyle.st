util
commentWithoutStyle
	^ '
CharacterScanner subclass: #CanvasCharacterScanner
	instanceVariableNames: ''canvas fillBlt foregroundColor runX lineY ''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''Morphic-Support''!

!CanvasCharacterScanner commentStamp: ''<historical>'' prior: 0!
A displaying scanner which draws its output to a Morphic canvas.!

!CanvasCharacterScanner methodsFor: ''stop conditions'' stamp: ''ar 12/15/2001 23:27''!
setStopConditions
	"Set the font and the stop conditions for the current run."

	self setFont.
	stopConditions
		at: Space asciiValue + 1
		put: (alignment = Justified ifTrue: [#paddedSpace])! !'
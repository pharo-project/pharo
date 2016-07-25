stop conditions
crossedX
	"There is a word that has fallen across the right edge of the composition 
	rectangle. This signals the need for wrapping which is done to the last 
	space that was encountered, as recorded by the space stop condition,
	or any other breakable character if the language permits so."

	pendingKernX := 0.
	
	lastBreakIsNotASpace ifTrue:
		["In some languages line break is possible before a non space."
		^self wrapAtLastBreakable].
	 
	spaceCount >= 1 ifTrue:
		["The common case. there is a space on the line."
		^self wrapAtLastSpace].
	
	"Neither internal nor trailing spaces -- almost never happens."
	self advanceIfFirstCharOfLine.
	^self wrapHere
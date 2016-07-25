blinking
onBlinkCursor
	"Blink the cursor"
	| para |
	para := self paragraph ifNil:[^nil].
	Time millisecondClockValue < self blinkStart ifTrue:[
		"don't blink yet"
		^para showCaret: para focused.
	].
	para showCaret: para showCaret not.
	para caretRect ifNotNil:[:r| self invalidRect: r].
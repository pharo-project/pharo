blinking
resetBlinkCursor
	"Reset the blinking cursor"
	| para |
	self blinkStart: Time millisecondClockValue + 500.
	para := self paragraph ifNil:[^self].
	para showCaret = para focused ifFalse:[
		para caretRect ifNotNil:[:r| self invalidRect: r].
		para showCaret: para focused.
	].

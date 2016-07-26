blinking
blinkStart
	"Reset time for blink cursor after which blinking should actually start"
	^self valueOfProperty: #blinkStart ifAbsent:[Time millisecondClockValue]

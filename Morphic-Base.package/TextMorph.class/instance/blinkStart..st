blinking
blinkStart: msecs
	"Reset time for blink cursor after which blinking should actually start"
	^self setProperty: #blinkStart toValue: msecs

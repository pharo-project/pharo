accessing
balloonText
	"Answer balloon help text or nil, if no help is available.  
	NB: subclasses may override such that they programatically  
	construct the text, for economy's sake, such as model phrases in 
	a Viewer"

	extension ifNil: [^nil].
	^extension balloonText ifNotNil: [:text | 
				text asString withNoLineLongerThan: self theme settings maxBalloonHelpLineLength]
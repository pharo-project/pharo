morphic ui
wantsRoundedCorners
	"Check to see if the property is set first."
	
	^self isFullscreen
		ifTrue: [false]
		ifFalse: [^super wantsRoundedCorners]
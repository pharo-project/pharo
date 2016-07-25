stepping
animateScrollingBackward: downPos

	"stop scrolling if slider reached cursor position"
	(downPos < mappedPosition) 
		ifTrue: [ self announcer announce: (TxSliderScrollBackward source: self) ]
		ifFalse: [ self stopStepping  ] 


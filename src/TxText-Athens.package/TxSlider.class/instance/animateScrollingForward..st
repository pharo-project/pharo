stepping
animateScrollingForward: downPos

	"stop scrolling if slider reached cursor position"
	(downPos > (mappedPosition+ mappedThickness)) 
		ifTrue: [ self announcer announce: (TxSliderScrollForward source: self) ]
		ifFalse: [ self stopStepping  ] 


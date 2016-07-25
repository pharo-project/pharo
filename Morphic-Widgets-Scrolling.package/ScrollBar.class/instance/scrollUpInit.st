access
scrollUpInit
	"Initialize a scroll up (from button) operation.
	Fixed to perform immediately with deferred 
	stepping for subsequent hold of button."

	|bc|
	bc := upButton borderStyle baseColor.
	upButton borderInset.
	upButton borderStyle baseColor: bc.
	self resetTimer.
	self scrollBarAction: #doScrollUp.
	self doScrollUp.
	self
		startStepping: #stepAt:
		at: Time millisecondClockValue + self stepTime
		arguments: nil stepTime: nil.
	upButton fillStyle: self pressedButtonFillStyle.
	upButton borderStyle: self pressedButtonBorderStyle
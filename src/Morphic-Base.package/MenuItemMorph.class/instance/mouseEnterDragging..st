events
mouseEnterDragging: evt
	"The mouse entered the receiver. Do nothing if we're not in a 'valid menu transition', meaning that the current hand focus must be aimed at the owning menu."
	evt hand mouseFocus == owner ifTrue:[owner selectItem: self event: evt]
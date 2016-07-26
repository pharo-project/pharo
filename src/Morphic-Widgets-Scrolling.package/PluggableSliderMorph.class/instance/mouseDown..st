event handling
mouseDown: anEvent
	"Set the value directly."
	
	self enabled ifTrue: [
		self
			scrollPoint: anEvent;
			computeSlider].
	super mouseDown: anEvent.
	self enabled ifFalse: [^self].
	anEvent hand newMouseFocus: slider event: anEvent.
	slider
		mouseEnter: anEvent copy;
		mouseDown: anEvent copy

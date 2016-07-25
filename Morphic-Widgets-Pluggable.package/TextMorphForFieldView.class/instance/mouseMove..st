event handling
mouseMove: evt
	"Allow auto scroll in any direction.
	Something else is preventing the left/right case."
	
	| editEvt |
	self perform: #mouseMove: withArguments: {evt} inSuperclass: TextMorph.
	evt redButtonPressed ifFalse: [^ self].
	editEvt := evt transformedBy: (self transformedFrom: editView) inverseTransformation.
	(editView bounds containsPoint: editEvt position) ifFalse:[
		"Start auto-scrolling"
		self startStepping: #autoScrollView:
			at: Time millisecondClockValue
			arguments: (Array with: editEvt)
			stepTime: 100. "fast enough"
	] ifTrue:[
		self stopSteppingSelector: #autoScrollView:.
	].
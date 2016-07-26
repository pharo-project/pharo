event handling
mouseMove: evt
	| editEvt |
	super mouseMove: evt.
	scrollPane ifNil: [ ^ self ].
	self scrollPivot: evt cursorPoint.
	editEvt := evt transformedBy: (self transformedFrom: scrollPane ) inverseTransformation.
	(editEvt position y between: scrollPane top and: scrollPane bottom)
		ifFalse: [
			self
				startStepping: #autoScroll:
				at: Time millisecondClockValue
				arguments: (Array with: evt)
				stepTime: 100	"fast enough" ]
		ifTrue: [ self stopSteppingSelector: #autoScroll: ]
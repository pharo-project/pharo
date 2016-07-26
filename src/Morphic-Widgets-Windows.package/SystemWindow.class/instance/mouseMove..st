events
mouseMove: evt
	"Handle a mouse-move event"

	self basicIsSticky ifFalse:[
		self fastFramingOn 
			ifTrue: [self doFastFrameDrag: evt startPoint ]
			ifFalse: [ self grabSelfOrTopRenderer: evt	]
	]. 
event handling
mouseDown: anEvent 
	scroller hasSubmorphs 
		ifTrue: [ self textArea mouseDown: (anEvent transformedBy: (scroller transformFrom: self)) ].
	self eventHandler
		ifNotNil: [ self eventHandler mouseDown: anEvent fromMorph: self ]
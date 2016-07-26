event handling
mouseMove: anEvent 

	scroller hasSubmorphs 
		ifTrue: [ self textArea mouseMove: (anEvent transformedBy: (scroller transformFrom: self)) ].
	self eventHandler
		ifNotNil: [ self eventHandler mouseMove: anEvent fromMorph: self ]
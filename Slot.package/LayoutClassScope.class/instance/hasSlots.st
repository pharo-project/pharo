testing
hasSlots
	self size > 0 
		ifTrue: [ ^ true ].
	^ parentScope hasSlots
testing
isOverridden
	self isActive
		ifTrue: [ ^ self compiledMethod isOverridden ].
	^ false
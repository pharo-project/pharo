testing
isAbstract
	self isActive
		ifTrue: [ ^ self compiledMethod isAbstract ].
	^ false
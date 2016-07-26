accessing
selectedMethod
	self selectedMessage ifNil: [ ^nil ].
	^ self selectedMessage compiledMethod
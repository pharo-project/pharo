testing
withFirstArgument: aBlock
	self arguments ifEmpty: [ ^ self ].
	^ aBlock value: self arguments first
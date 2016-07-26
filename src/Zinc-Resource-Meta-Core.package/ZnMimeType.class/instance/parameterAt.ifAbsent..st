parameters
parameterAt: key ifAbsent: block
	parameters isNil ifTrue: [ ^ block value ].
	^ self parameters at: key ifAbsent: block
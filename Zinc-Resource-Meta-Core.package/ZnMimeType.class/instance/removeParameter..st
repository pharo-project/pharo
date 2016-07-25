parameters
removeParameter: key
	parameters isNil ifTrue: [ ^ self ].
	^ self parameters removeKey: key ifAbsent: [ nil ]
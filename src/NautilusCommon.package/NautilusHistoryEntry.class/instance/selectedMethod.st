accessing
selectedMethod
	selectedSelector ifNil: [ ^ nil ].
	^ selectedClass ifNil: [ nil ] ifNotNil: [ selectedClass compiledMethodAt: selectedSelector ifAbsent: [ nil ] ]
tests
testSingleMethod
	| old new |
	old := HEMethod for: (Behavior >> #basicNew:).
	old writeInto: writer.
	writer flush.
	
	new := HEMethod readFrom: reader.

	self assert: old name equals: #basicNew:.
	self assert: old equals: new.

	self assert: (old literals at:12) species equals: HEUndefinedObject.
	self assert: ((old literals at:11) value value at:1) keyword value equals: #primitive:error:
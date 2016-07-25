accessing
selectedClass
	classSelection ifNil: [ ^ nil ].
	^ Smalltalk globals at: classSelection ifAbsent: [ nil ]
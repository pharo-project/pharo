subclassresponsibility
selectedClass
	| definition |
	selection ifNil: [ ^ nil ].
	(definition := selection definition) ifNil: [ ^ nil ].
	definition isMethodDefinition
		ifFalse: [ ^ nil ].
	^ Smalltalk globals at: definition className ifAbsent: [  ]
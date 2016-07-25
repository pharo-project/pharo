accessing
selectedClassOrMetaClass
	| class |
	classSelection ifNil: [ ^ nil ].
	class := Smalltalk globals at: classSelection ifAbsent: [ ^ nil ].
	^ self switchIsClass
		ifTrue: [ class class ]
		ifFalse: [ class ]
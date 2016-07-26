adding/removing
addRejectClass: aClass

	|  falsePositives |
	falsePositives := self manifest rejectClasses asOrderedCollection .
	((self unwrap: falsePositives) anySatisfy: [:each | each = aClass])
		ifTrue: [ ^ self ].
	falsePositives add: {aClass asRingDefinition arrayStringForManifest. DateAndTime current asString}.
	self compileSelector:  (self class rejectClassesTag) returnValue: (self buildArrayString: falsePositives)
	
	
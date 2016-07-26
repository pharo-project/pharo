annotations
annotationNames
	
	self hasAnnotations
		ifFalse:[ ^OrderedCollection new ].
	^annotations keys
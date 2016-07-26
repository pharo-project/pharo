annotations
hasAnnotationNamed: annotationName
	"Answer <true> if the receiver contains the annotation named <annotationName>. "
	
	self hasAnnotations	
		ifFalse:[ ^false ].
	^annotations includesKey: annotationName
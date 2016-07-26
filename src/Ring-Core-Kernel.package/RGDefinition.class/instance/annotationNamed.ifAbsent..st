annotations
annotationNamed: annotationName ifAbsent: exceptionBlock 
	"Answer the value of the annotation named <annotationName>. If the 
	annotation is not defined for the receiver, answer the result of 
	evaluating the <exceptionBlock>."

	self hasAnnotations
		ifFalse: [ ^exceptionBlock value ].

	^self annotations
			at: annotationName
			ifAbsent: [ exceptionBlock value ]
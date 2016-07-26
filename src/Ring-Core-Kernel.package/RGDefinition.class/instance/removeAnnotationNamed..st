annotations
removeAnnotationNamed: annotationName 
	"Remove the annotation named <annotationName>. Fails quietly if there 
	is no annotation with the given name."
	
	(self hasAnnotationNamed: annotationName)
		ifTrue:[ annotations removeKey: annotationName ]
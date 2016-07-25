source pointers
sourcePointer
	"Retrieves the sourcePointer for this definition if exists"
	
	^ self isActive 
		ifTrue: [ self compiledMethod sourcePointer ]
		ifFalse: [ self annotationNamed: self class sourcePointerKey ]
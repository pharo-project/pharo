Class {
	#name : #ContextualClassAnnotationExample1,
	#superclass : #ClassAnnotation,
	#category : #'ClassAnnotation-Tests-Examples-Annotated classes'
}

{ #category : #testing }
ContextualClassAnnotationExample1 >> isActiveInContext: anAnnotationUser [
	^(super isActiveInContext: anAnnotationUser)
		and: [ anAnnotationUser isSimulatedActiveAnnotation ]
]

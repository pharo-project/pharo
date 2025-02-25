Class {
	#name : 'ForbiddenClassAnnotationExample',
	#superclass : 'ClassAnnotation',
	#category : 'ClassAnnotation-Tests-Examples-Annotated classes',
	#package : 'ClassAnnotation-Tests',
	#tag : 'Examples-Annotated classes'
}

{ #category : 'testing' }
ForbiddenClassAnnotationExample >> isForbidden [
	self assert: annotatedClass isNotNil.
	^ true
]

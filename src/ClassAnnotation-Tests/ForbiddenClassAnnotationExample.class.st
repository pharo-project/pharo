Class {
	#name : #ForbiddenClassAnnotationExample,
	#superclass : #ClassAnnotation,
	#category : #'ClassAnnotation-Tests-Examples-Annotated classes'
}

{ #category : #testing }
ForbiddenClassAnnotationExample >> isForbidden [
	self assert: annotatedClass notNil.
	^true
]

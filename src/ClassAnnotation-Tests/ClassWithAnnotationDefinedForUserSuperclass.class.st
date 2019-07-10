Class {
	#name : #ClassWithAnnotationDefinedForUserSuperclass,
	#superclass : #Object,
	#category : #'ClassAnnotation-Tests-Examples-Annotated classes'
}

{ #category : #annotations }
ClassWithAnnotationDefinedForUserSuperclass class >> annotationExample [
	<classAnnotation>
	
	^ContextualClassAnnotationExample2 for: BaseAnnotationUserExample
]

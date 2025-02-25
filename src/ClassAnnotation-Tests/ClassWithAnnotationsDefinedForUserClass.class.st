Class {
	#name : 'ClassWithAnnotationsDefinedForUserClass',
	#superclass : 'Object',
	#category : 'ClassAnnotation-Tests-Examples-Annotated classes',
	#package : 'ClassAnnotation-Tests',
	#tag : 'Examples-Annotated classes'
}

{ #category : 'annotations' }
ClassWithAnnotationsDefinedForUserClass class >> annotationExample1 [
	<classAnnotation>

	^ContextualClassAnnotationExample1 for: AnnotationUserExample
]

{ #category : 'annotations' }
ClassWithAnnotationsDefinedForUserClass class >> annotationExample2 [
	<classAnnotation>

	^ContextualClassAnnotationExample1 for: AnnotationUserExample
]

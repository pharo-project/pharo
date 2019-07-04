Class {
	#name : #ClassWithSingleAnnotation,
	#superclass : #Object,
	#category : #'ClassAnnotation-Tests-Examples-Annotated classes'
}

{ #category : #annotations }
ClassWithSingleAnnotation class >> annotationExample [
	<classAnnotation>
	
	^ClassAnnotationExample3 new
]

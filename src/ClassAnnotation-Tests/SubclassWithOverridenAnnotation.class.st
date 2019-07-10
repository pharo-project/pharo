Class {
	#name : #SubclassWithOverridenAnnotation,
	#superclass : #ClassWithSingleAnnotation,
	#category : #'ClassAnnotation-Tests-Examples-Annotated classes'
}

{ #category : #annotations }
SubclassWithOverridenAnnotation class >> annotationExample [
	<classAnnotation>
	
	^ClassAnnotationExample2 new
]

Class {
	#name : #ClassWithThreeAnnotations,
	#superclass : #Object,
	#category : #'ClassAnnotation-Tests-Examples-Annotated classes'
}

{ #category : #annotations }
ClassWithThreeAnnotations class >> annotationExample1 [
	<classAnnotation>
	
	^ClassAnnotationExample1 new
]

{ #category : #annotations }
ClassWithThreeAnnotations class >> annotationExample2 [
	<classAnnotation>
	
	^ClassAnnotationExample1 new
]

{ #category : #annotations }
ClassWithThreeAnnotations class >> annotationExample3 [
	<classAnnotation>
	
	^ClassAnnotationExample2 new
]

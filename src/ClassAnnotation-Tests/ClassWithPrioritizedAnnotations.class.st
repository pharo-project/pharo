Class {
	#name : #ClassWithPrioritizedAnnotations,
	#superclass : #Object,
	#category : #'ClassAnnotation-Tests-Examples-Annotated classes'
}

{ #category : #annotations }
ClassWithPrioritizedAnnotations class >> annotationWithPriority1 [
	<classAnnotation>
	
	^ClassAnnotationExampleWithPriority new priority: 10
]

{ #category : #annotations }
ClassWithPrioritizedAnnotations class >> annotationWithPriority2 [
	<classAnnotation>
	
	^ClassAnnotationExampleWithPriority new priority: 20
]

{ #category : #annotations }
ClassWithPrioritizedAnnotations class >> annotationWithPriority3 [
	<classAnnotation>
	
	^ClassAnnotationExampleWithPriority new priority: 15
]

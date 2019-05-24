Class {
	#name : #ClassWithForbiddenAnnotationExample,
	#superclass : #Object,
	#category : #'ClassAnnotation-Tests-Examples-Annotated classes'
}

{ #category : #annotations }
ClassWithForbiddenAnnotationExample class >> failedForbiddenAnnotation [
	<classAnnotation>
	
	^ClassAnnotationExampleWithFailedForbiddenCheck new
]

{ #category : #annotations }
ClassWithForbiddenAnnotationExample class >> forbiddenAnnotation [
	<classAnnotation>
	
	^ForbiddenClassAnnotationExample new
]

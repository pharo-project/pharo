Class {
	#name : #ClassWithBrokenAnnotatingMethod,
	#superclass : #Object,
	#category : #'ClassAnnotation-Tests-Examples-Annotated classes'
}

{ #category : #annotations }
ClassWithBrokenAnnotatingMethod class >> brokenAnnotation [
	<classAnnotation>
	
	self error: self brokenAnnotationMessage
]

{ #category : #annotations }
ClassWithBrokenAnnotatingMethod class >> brokenAnnotationMessage [
	^'This is example of failed class annotation method'
]

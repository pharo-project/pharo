Class {
	#name : 'ClassAnnotationExampleWithFailedForbiddenCheck',
	#superclass : 'ClassAnnotation',
	#category : 'ClassAnnotation-Tests-Examples-Annotations',
	#package : 'ClassAnnotation-Tests',
	#tag : 'Examples-Annotations'
}

{ #category : 'testing' }
ClassAnnotationExampleWithFailedForbiddenCheck >> isForbidden [
	self error: 'Failed forbidden check should lead to the BrokenClassAnnotation instance'
]

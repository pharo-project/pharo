Class {
	#name : 'AnnotationUserExample',
	#superclass : 'BaseAnnotationUserExample',
	#instVars : [
		'isSimulatedActiveAnnotation'
	],
	#category : 'ClassAnnotation-Tests-Examples-Annotation contexts',
	#package : 'ClassAnnotation-Tests',
	#tag : 'Examples-Annotation contexts'
}

{ #category : 'initialization' }
AnnotationUserExample >> initialize [
	super initialize.

	isSimulatedActiveAnnotation := true
]

{ #category : 'accessing' }
AnnotationUserExample >> isSimulatedActiveAnnotation [
	^ isSimulatedActiveAnnotation
]

{ #category : 'accessing' }
AnnotationUserExample >> isSimulatedActiveAnnotation: anObject [
	isSimulatedActiveAnnotation := anObject
]

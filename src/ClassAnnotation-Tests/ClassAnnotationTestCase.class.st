Class {
	#name : #ClassAnnotationTestCase,
	#superclass : #TestCase,
	#category : #'ClassAnnotation-Tests-Base'
}

{ #category : #testing }
ClassAnnotationTestCase class >> isAbstract [
	^self == ClassAnnotationTestCase
]

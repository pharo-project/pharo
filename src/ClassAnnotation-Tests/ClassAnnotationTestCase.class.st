Class {
	#name : #ClassAnnotationTestCase,
	#superclass : #TestCase,
	#category : #'ClassAnnotation-Tests'
}

{ #category : #testing }
ClassAnnotationTestCase class >> isAbstract [
	^self == ClassAnnotationTestCase
]

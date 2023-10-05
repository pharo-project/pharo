Class {
	#name : 'ClassAnnotationTestCase',
	#superclass : 'TestCase',
	#category : 'ClassAnnotation-Tests-Base',
	#package : 'ClassAnnotation-Tests',
	#tag : 'Base'
}

{ #category : 'testing' }
ClassAnnotationTestCase class >> isAbstract [
	^self == ClassAnnotationTestCase
]

Class {
	#name : #RBMethodProtocolTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #testing }
RBMethodProtocolTransformationTest >> testClassDoesNotExist [

	self shouldFail: (RBMethodProtocolTransformation
							protocol: 'empty protocol 2'
							inMethod: #someMethod
							inClass: #RBDummyEmptyClass123)
							asRefactoring
]

{ #category : #testing }
RBMethodProtocolTransformationTest >> testMethodDoesNotExist [

	self shouldFail: (RBMethodProtocolTransformation
							protocol: 'empty protocol 2'
							inMethod: #method
							inClass: #RBDummyEmptyClass)
							asRefactoring
]

{ #category : #testing }
RBMethodProtocolTransformationTest >> testRefactoring [

	| refactoring |
	self assert: ( RBDummyEmptyClass
		organization protocolOrganizer
		protocolNamed: 'empty protocol 2' ) isEmpty.
	
	refactoring := (RBMethodProtocolTransformation
						protocol: 'empty protocol 2'
						inMethod: #someMethod
						inClass: #RBDummyEmptyClass)
						asRefactoring transform.
	RBRefactoringManager instance addRefactoring: refactoring.

	self assert: ( RBDummyEmptyClass
		organization protocolOrganizer
		protocolNamed: 'empty protocol 2' ) isEmpty not.
	
	refactoring := (RBMethodProtocolTransformation 
						protocol: 'empty protocol 1'
						inMethod: #someMethod
						inClass: #RBDummyEmptyClass)
						asRefactoring transform.
	RBRefactoringManager instance addRefactoring: refactoring.
		
	self assert: ( RBDummyEmptyClass
		organization protocolOrganizer
		protocolNamed: 'empty protocol 2' ) isEmpty.
]

{ #category : #testing }
RBMethodProtocolTransformationTest >> testTransform [

	| transformation |
	self assert: ( RBDummyEmptyClass
		organization protocolOrganizer
		protocolNamed: 'empty protocol 2' ) isEmpty.
	
	transformation := (RBMethodProtocolTransformation new
		protocol: 'empty protocol 2'
		inMethod: #someMethod
		inClass: #RBDummyEmptyClass)
		transform.
	RBRefactoringManager instance addRefactoring: transformation.

	self assert: ( RBDummyEmptyClass
		organization protocolOrganizer
		protocolNamed: 'empty protocol 2' ) isEmpty not.
	
	transformation := (RBMethodProtocolTransformation new
		protocol: 'empty protocol 1'
		inMethod: #someMethod
		inClass: #RBDummyEmptyClass)
		transform.
	RBRefactoringManager instance addRefactoring: transformation.
		
	self assert: ( RBDummyEmptyClass
		organization protocolOrganizer
		protocolNamed: 'empty protocol 2' ) isEmpty.
]

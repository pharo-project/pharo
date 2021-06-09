Class {
	#name : #RBRemoveTemporaryVariableTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #tests }
RBRemoveTemporaryVariableTransformationTest >> testClassDoesNotExist [

	self shouldFail: (RBRemoveTemporaryVariableTransformation 
							variable: 'temp'
							inMethod: #foo
							inClass: #RBTemporaryVariableTransformationTest)
							asRefactoring 
]

{ #category : #tests }
RBRemoveTemporaryVariableTransformationTest >> testMethodDoesNotExist [

	self shouldFail: (RBRemoveTemporaryVariableTransformation 
							variable: 'temp'
							inMethod: #foofoo
							inClass: #RBRemoveTemporaryVariableTransformationTest)
							asRefactoring 
]

{ #category : #tests }
RBRemoveTemporaryVariableTransformationTest >> testRefactoring [

	| refactoring class |
	refactoring := (RBAddMethodTransformation 
						sourceCode: 'foo
										| temp bar |
										bar := 5.
										temp := bar * bar.
										Transcript show: temp printString; cr.
										^temp * temp'
						in: #RBRemoveTemporaryVariableTransformationTest
						withProtocol: #accessing)
						asRefactoring transform.
	
	refactoring := (RBRemoveTemporaryVariableTransformation 
						model: refactoring model
						variable: 'temp'
						inMethod: #foo
						inClass: #RBRemoveTemporaryVariableTransformationTest)
						asRefactoring transform.
	
	self assert: refactoring model changes changes size equals: 2.			
	
	class := refactoring model classNamed: #RBRemoveTemporaryVariableTransformationTest.
	self assert: (class directlyDefinesMethod: #foo).		
	self assert: (class parseTreeFor: #foo) temporaries size equals: 1
]

{ #category : #tests }
RBRemoveTemporaryVariableTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBAddMethodTransformation
					sourceCode: 'foo
									| temp bar |
									bar := 5.
									temp := bar * bar.
									Transcript show: temp printString; cr.
									^temp * temp'
					in: self changeMock name
					withProtocol: #accessing)
					transform.
	
	transformation := (RBRemoveTemporaryVariableTransformation 
				model: transformation model
				variable: 'temp'
				inMethod: #foo
				inClass: self changeMock name)
				transform.
	
	self assert: transformation model changes changes size equals: 2.			
	
	class := transformation model classNamed: self changeMock name.
	self assert: (class directlyDefinesMethod: #one).		
	self assert: (class parseTreeFor: #foo) temporaries size equals: 1
]

{ #category : #tests }
RBRemoveTemporaryVariableTransformationTest >> testVariableDoesNotExist [

	self shouldFail: (RBRemoveTemporaryVariableTransformation 
							variable: 'temp123'
							inMethod: #foo
							inClass: #RBRemoveTemporaryVariableTransformationTest)
							asRefactoring 
]

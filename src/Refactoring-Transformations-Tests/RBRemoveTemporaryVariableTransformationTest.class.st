Class {
	#name : 'RBRemoveTemporaryVariableTransformationTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'tests - failures' }
RBRemoveTemporaryVariableTransformationTest >> testClassDoesNotExist [

	self shouldFail: (RBRemoveTemporaryVariableTransformation
			 variable: 'temp'
			 inMethod: #foo
			 inClass: #RBTemporaryVariableTransformationTest)
]

{ #category : 'tests - failures' }
RBRemoveTemporaryVariableTransformationTest >> testMethodDoesNotExist [

	self shouldFail: (RBRemoveTemporaryVariableTransformation
			 variable: 'temp'
			 inMethod: #foofoo
			 inClass: #RBRemoveTemporaryVariableTransformationTest)
]

{ #category : 'tests' }
RBRemoveTemporaryVariableTransformationTest >> testTransform [

	| transformation class |
	transformation := RBAddMethodTransformation
		sourceCode: 'foo
			| temp bar unused |
			bar := 5.
			temp := bar * bar.
			Transcript show: temp printString; cr.
			^temp * temp'
		in: self changeMockClass name
		withProtocol: #accessing.
	transformation generateChanges.

	transformation := RBRemoveTemporaryVariableTransformation
		model: transformation model
		variable: 'unused'
		inMethod: #foo
		inClass: self changeMockClass name.
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 2.
	class := transformation model classNamed: self changeMockClass name.
	self assert: (class parseTreeForSelector: #foo) temporaries size equals: 2
]

{ #category : 'tests - failures' }
RBRemoveTemporaryVariableTransformationTest >> testVariableDoesNotExist [

	self shouldFail: (RBRemoveTemporaryVariableTransformation
			 variable: 'temp123'
			 inMethod: #foo
			 inClass: #RBRemoveTemporaryVariableTransformationTest)
]

{ #category : 'tests' }
RBRemoveTemporaryVariableTransformationTest >> testVariableHasUsagesWithoutShadowing [

	| transformation |
	transformation := RBAddMethodTransformation
		sourceCode: 'foo
			| temp bar |
			bar := 5.
			temp := bar * bar.
			^temp'
		in: self changeMockClass name
		withProtocol: #accessing.
	transformation generateChanges.

	transformation := RBRemoveTemporaryVariableTransformation
		model: transformation model
		variable: 'temp'
		inMethod: #foo
		inClass: self changeMockClass name.
		
	self should: [ transformation generateChanges ] raise: RBRefactoringError
]

{ #category : 'tests' }
RBRemoveTemporaryVariableTransformationTest >> testVariableHasUsagesWithShadowing [

	| transformation class |
	transformation := RBAddVariableTransformation
		instanceVariable: 'temp'
		class: self changeMockClass name.
	transformation generateChanges.

	transformation := RBAddMethodTransformation
		model: transformation model
		sourceCode: 'foo
			| temp bar |
			bar := 5.
			temp := bar * bar.
			^temp'
		in: self changeMockClass name
		withProtocol: #accessing.
	transformation generateChanges.

	transformation := RBRemoveTemporaryVariableTransformation
		model: transformation model
		variable: 'temp'
		inMethod: #foo
		inClass: self changeMockClass name.
	transformation generateChanges.

	class := transformation model classNamed: self changeMockClass name.
	self assert: (class parseTreeForSelector: #foo) temporaries size equals: 1
]
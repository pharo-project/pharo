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

{ #category : 'tests' }
RBRemoveTemporaryVariableTransformationTest >> testDefiningSequenceNodeAndHasUsagesInScope [

	| transformation |
	transformation := RBAddMethodTransformation
		sourceCode: 'foo
			| unused used |
			used := 1.
			^used'
		in: self changeMockClass name
		withProtocol: #accessing.
	transformation generateChanges.

	"Test helper methods on unused temp"
	transformation := RBRemoveTemporaryVariableTransformation
		model: transformation model
		variable: 'unused'
		inMethod: #foo
		inClass: self changeMockClass name.

	self assert: transformation definingSequenceNode isNotNil.
	self deny: transformation hasUsagesInScope.

	"Test helper methods on used temp"
	transformation := RBRemoveTemporaryVariableTransformation
		model: transformation model
		variable: 'used'
		inMethod: #foo
		inClass: self changeMockClass name.

	self assert: transformation definingSequenceNode isNotNil.
	self assert: transformation hasUsagesInScope
]

{ #category : 'tests - failures' }
RBRemoveTemporaryVariableTransformationTest >> testMethodDoesNotExist [

	self shouldFail: (RBRemoveTemporaryVariableTransformation
			 variable: 'temp'
			 inMethod: #foofoo
			 inClass: #RBRemoveTemporaryVariableTransformationTest)
]

{ #category : 'tests' }
RBRemoveTemporaryVariableTransformationTest >> testRemoveOuterUnusedTemporaryWhenInnerBlockShadowsSameName [

	| transformation class |
	transformation := RBAddMethodTransformation
		sourceCode: 'foo
			| sameName |
			[ :x | | sameName | sameName := x * 2. sameName ] value: 5.
			^42'
		in: self changeMockClass name
		withProtocol: #accessing.
	transformation generateChanges.

	transformation := RBRemoveTemporaryVariableTransformation
		model: transformation model
		variable: 'sameName'
		inMethod: #foo
		inClass: self changeMockClass name.
	transformation generateChanges.

	class := transformation model classNamed: self changeMockClass name.
	self assert: (class parseTreeForSelector: #foo) temporaries isEmpty
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
RBRemoveTemporaryVariableTransformationTest >> testVariableShadowedByBlockArgumentNotUsed [

	| transformation class |
	transformation := RBAddMethodTransformation
		sourceCode: 'foo
			| temp |
			[ :temp | temp ] value: 1.
			^42'
		in: self changeMockClass name
		withProtocol: #accessing.
	transformation generateChanges.

	transformation := RBRemoveTemporaryVariableTransformation
		model: transformation model
		variable: 'temp'
		inMethod: #foo
		inClass: self changeMockClass name.

	self deny: transformation hasUsagesInScope.
	transformation generateChanges.

	class := transformation model classNamed: self changeMockClass name.
	self assert: (class parseTreeForSelector: #foo) temporaries isEmpty
]

{ #category : 'tests' }
RBRemoveTemporaryVariableTransformationTest >> testVariableUsedDirectly [

	| transformation |
	transformation := RBAddMethodTransformation
		sourceCode: 'foo
			| temp |
			temp'
		in: self changeMockClass name
		withProtocol: #accessing.
	transformation generateChanges.

	transformation := RBRemoveTemporaryVariableTransformation
		model: transformation model
		variable: 'temp'
		inMethod: #foo
		inClass: self changeMockClass name.

	self assert: transformation hasUsagesInScope.
	self should: [ transformation generateChanges ] raise: RBRefactoringError
]

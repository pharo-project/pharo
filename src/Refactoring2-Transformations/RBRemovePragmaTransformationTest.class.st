Class {
	#name : #RBRemovePragmaTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #'testing-utilities' }
RBRemovePragmaTransformationTest >> methodAfter [

	| variable |
	variable := 'String'.
	variable byteAt: 1.
]

{ #category : #'testing-utilities' }
RBRemovePragmaTransformationTest >> methodBefore [
	<gtInspectorPresentationOrder: 34>

	| variable |
	variable := 'String'.
	variable byteAt: 1.
]

{ #category : #testing }
RBRemovePragmaTransformationTest >> testClassDoesNotExist [

	self shouldFail: (RBRemovePragmaTransformation 
							pragma: '<gtInspectorPresentationOrder: 34>'
							inMethod: #methodBefore
							inClass: #RBPragmaTransformationTest)
							asRefactoring 
]

{ #category : #testing }
RBRemovePragmaTransformationTest >> testMethodDoesNotExist [

	self shouldFail: (RBRemovePragmaTransformation 
							pragma: '<gtInspectorPresentationOrder: 34>'
							inMethod: #method
							inClass: #RBRemovePragmaTransformationTest)
							asRefactoring 
]

{ #category : #testing }
RBRemovePragmaTransformationTest >> testPragmaDoesNotExist [

	self shouldFail: (RBRemovePragmaTransformation 
							pragma: '<gtPresentationOrder: 34>'
							inMethod: #methodBefore
							inClass: #RBRemovePragmaTransformationTest)
							asRefactoring
]

{ #category : #testing }
RBRemovePragmaTransformationTest >> testRefactoring [

	| refactoring class |
	refactoring := (RBRemovePragmaTransformation 
						pragma: '<gtInspectorPresentationOrder: 34>'
						inMethod: #methodBefore
						inClass: #RBRemovePragmaTransformationTest)
						asRefactoring transform.
	
	self assert: refactoring model changes changes size equals: 1.
	
	class := refactoring model classNamed: #RBRemovePragmaTransformationTest.
	self assert: (class parseTreeFor: #methodBefore) body
			equals: (class parseTreeFor: #methodAfter) body.
]

{ #category : #testing }
RBRemovePragmaTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBRemovePragmaTransformation new
		pragma: '<gtInspectorPresentationOrder: 34>'
		inMethod: #methodBefore
		inClass: self class name)
		transform.
	
	self assert: transformation model changes changes size equals: 1.
	
	class := transformation model classNamed: self class name.
	self assert: (class parseTreeFor: #methodBefore) body
			equals: (class parseTreeFor: #methodAfter) body.
]

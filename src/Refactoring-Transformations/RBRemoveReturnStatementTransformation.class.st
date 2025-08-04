"
I am responsible for removing a return statement inside a method.

Usage:
```
| transformation |
transformation := (RBRemoveReturnStatementTransformation
			return: '^ variable'
			inMethod: #methodBefore
			inClass: #RBRemoveReturnStatementTransformationTest)
			generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
```

Preconditions:
- Checks whether the method has a return.

Observations about the transformation:
- This transformation is straightforward: just remove the last node of the method.
- In Pharo, return statements can also be declared inside blocks. In this case, the transformation will try to match return statement passed as argument and remove the return values in this method. If no matching return value is found, the transformation will remove the last node.
"
Class {
	#name : 'RBRemoveReturnStatementTransformation',
	#superclass : 'RBMethodTransformation',
	#instVars : [
		'returnValue'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'api' }
RBRemoveReturnStatementTransformation class >> model: aRBModel return: anExpression inMethod: aSelector inClass: aClassName [

	^ self new
		model: aRBModel;
		return: anExpression
		inMethod: aSelector
		inClass: aClassName;
		yourself
]

{ #category : 'api' }
RBRemoveReturnStatementTransformation class >> return: anExpression inMethod: aSelector inClass: aClassName [

	^ self new
		return: anExpression
		inMethod: aSelector
		inClass: aClassName;
		yourself
]

{ #category : 'preconditions' }
RBRemoveReturnStatementTransformation >> applicabilityPreconditions [

	^ {
		  self classExist.
		  (RBCondition
			   withBlock: [ self definingClass canUnderstand: selector ]
			   errorString:
			   ('Method named <1s> does not exist' expandMacrosWith: selector)).
		  (RBCondition
			   withBlock: [ self definingMethod lastIsReturn ]
			   errorString:
				   ('Method named <1s> does not have a return statement'
					    expandMacrosWith: selector)) }
]

{ #category : 'scripting api - conditions' }
RBRemoveReturnStatementTransformation >> checkPreconditions [ 

	self eagerlyCheckApplicabilityPreconditions 
]

{ #category : 'preconditions' }
RBRemoveReturnStatementTransformation >> classExist [

	^ RBCondition
		  withBlock: [ self definingClass isNotNil ]
		  errorString:
		  ('Class named <1s> does not exist' expandMacrosWith: className)
]

{ #category : 'executing' }
RBRemoveReturnStatementTransformation >> privateTransform [

	| methodTree nodesToRemove |
	methodTree := self definingMethod.

	nodesToRemove := ( methodTree allChildren removeDuplicates
		select: #isReturn )
		select: [ :node | node sourceCode = returnValue ].

	nodesToRemove
		ifEmpty: [ methodTree removeLast ]
		ifNotEmpty: [ nodesToRemove do: [ :node | node parent removeNode: node ] ].
	self definingClass compileTree: methodTree
]

{ #category : 'api' }
RBRemoveReturnStatementTransformation >> return: anExpression inMethod: aSelector inClass: aClassName [

	self className: aClassName.
	selector := aSelector.
	returnValue := anExpression
]

{ #category : 'storing' }
RBRemoveReturnStatementTransformation >> storeOn: aStream [

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' return: ''';
		nextPutAll: returnValue;
		nextPutAll: ''' inMethod: ''';
		nextPutAll: selector;
		nextPutAll: ''' inClass: '.
	class storeOn: aStream.
	aStream nextPut: $)
]

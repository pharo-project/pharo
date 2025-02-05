"
I am responsible for adding a temporary variable inside a method. In Pharo, temporary variables can be declared inside blocks as well. By default, the container is the method itself.

Usage:
```
| transformation |
transformation := (RBAddTemporaryVariableTransformation
			variable: #newVariable
			inMethod: #methodBefore
			inClass: #RBAddReturnStatementTransformationTest)
			generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
```

Preconditions:
- Check whether there is a variable inside the method with the same name.

Observations about the transformation:
- In this implementation, the temporary variable will be added to the body of the method
"
Class {
	#name : 'RBAddTemporaryVariableTransformation',
	#superclass : 'RBMethodTransformation',
	#instVars : [
		'variableName',
		'interval'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'api' }
RBAddTemporaryVariableTransformation class >> model: aRBModel variable: aString inMethod: aSelector inClass: aClass [

	^ self new
		model: aRBModel;
		variable: aString
		inMethod: aSelector
		inClass: aClass;
		yourself
]

{ #category : 'api' }
RBAddTemporaryVariableTransformation class >> variable: aString inInterval: anInterval inMethod: aSelector inClass: aClass [

	^ self new
		variable: aString
		inInterval: anInterval
		inMethod: aSelector
		inClass: aClass;
		yourself
]

{ #category : 'api' }
RBAddTemporaryVariableTransformation class >> variable: aString inMethod: aSelector inClass: aClass [

	^ self new
		variable: aString
		inMethod: aSelector
		inClass: aClass;
		yourself
]

{ #category : 'preconditions' }
RBAddTemporaryVariableTransformation >> applicabilityPreconditions [

	^ {
		  (RBCondition
			   withBlock: [ self definingClass isNotNil ]
			   errorString: 'No such class or trait named ' , className).
		  (RBCondition definesSelector: selector in: self definingClass).
		  (RBCondition
			   withBlock: [
				   (self definingMethod allArgumentVariables includes:
					    variableName) not ]
			   errorString:
				   'Method named ' , selector
				   , ' already defines an argument named ' , variableName) }
]

{ #category : 'scripting api - conditions' }
RBAddTemporaryVariableTransformation >> checkPreconditions [ 

	self eagerlyCheckApplicabilityPreconditions 
]

{ #category : 'executing' }
RBAddTemporaryVariableTransformation >> privateTransform [

	| methodTree parseTree |
	methodTree := self definingMethod.
	parseTree := methodTree body.

	interval ifNotNil: [
		| nodeInInterval |
		nodeInInterval := methodTree bestNodeFor: interval.
		nodeInInterval isSequence ifTrue: [ parseTree := nodeInInterval ] ].

	self flag: #skippingPreconditions.
	(parseTree allTemporaryVariables includes: variableName) ifTrue: [ ^ self ].
	
	parseTree addTemporaryNamed: variableName.
	class compileTree: methodTree
]

{ #category : 'preconditions' }
RBAddTemporaryVariableTransformation >> skippingPreconditions [

	^ RBCondition
		  withBlock: [
			  (self definingMethod allTemporaryVariables includes: variableName)
				  not ]
		  errorString: 'Method named ' , selector
			  , ' already defines a temporary variable named ' , variableName
]

{ #category : 'storing' }
RBAddTemporaryVariableTransformation >> storeOn: aStream [

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' variable: ''';
		nextPutAll: variableName;
		nextPutAll: ''' inInterval: ''';
		nextPutAll: interval;
		nextPutAll: ''' inMethod: ''';
		nextPutAll: selector;
		nextPutAll: ''' inClass: '.
	class storeOn: aStream.
	aStream nextPut: $)
]

{ #category : 'api' }
RBAddTemporaryVariableTransformation >> variable: aVariableName inInterval: anInterval inMethod: aSelector inClass: aClass [

	self className: aClass.
	interval := anInterval.
	selector := aSelector.
	variableName := aVariableName
]

{ #category : 'api' }
RBAddTemporaryVariableTransformation >> variable: aVariableName inMethod: aSelector inClass: aClass [

	self
		variable: aVariableName
		inInterval: nil
		inMethod: aSelector
		inClass: aClass
]

"
RBRemoveTemporaryVariableRefactoring is responsible for removing temporary variables inside a method. In Pharo, temporary variables can be declared inside blocks as well.

Example:
```
| transformation |
transformation := (RBRemoveTemporaryVariableTransformation
	variable: 'methodTree'
	inMethod: #privateTransform
	inClass: #RBRemoveTemporaryVariableTransformation)
			generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
```
	
Preconditions:
- Check whether this method has a temporary variable with the given name.

Observations about the transformation:
- The transformation looks for the variable inside blocks too
"
Class {
	#name : 'RBRemoveTemporaryVariableTransformation',
	#superclass : 'RBMethodTransformation',
	#instVars : [
		'variableName'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'api' }
RBRemoveTemporaryVariableTransformation class >> model: aRBModel variable: aString inMethod: aSelector inClass: aClass [

	^ self new
		model: aRBModel;
		variable: aString
		inMethod: aSelector
		inClass: aClass;
		yourself
]

{ #category : 'api' }
RBRemoveTemporaryVariableTransformation class >> variable: aString inMethod: aSelector inClass: aClass [

	^ self new
		variable: aString
		inMethod: aSelector
		inClass: aClass;
		yourself
]

{ #category : 'preconditions' }
RBRemoveTemporaryVariableTransformation >> applicabilityPreconditions [

	^ {
		  self classExist.
		  (RBCondition definesSelector: selector in: self definingClass).
		  (RBCondition
			   withBlock: [
			   self definingMethod allTemporaryVariables includes: variableName ]
			   errorString: 'Method named ' , selector
				   , ' does not define a temporary variable named ' , variableName).
		  (RBCondition
			   withBlock: [
				   (self definingMethod allArgumentVariables includes:
					    variableName) not ]
			   errorString: 'Variable named ' , variableName
				   , ' cannot be removed because it is an argument in this method') }
]

{ #category : 'scripting api - conditions' }
RBRemoveTemporaryVariableTransformation >> checkPreconditions [ 

	self eagerlyCheckApplicabilityPreconditions 
]

{ #category : 'preconditions' }
RBRemoveTemporaryVariableTransformation >> classExist [

	^ RBCondition
		  withBlock: [ self definingClass isNotNil ]
		  errorString: 'No such class or trait named ' , className asString
]

{ #category : 'executing' }
RBRemoveTemporaryVariableTransformation >> privateTransform [

	| methodTree modifiedNode |
	methodTree := self definingMethod.

	modifiedNode := (methodTree allChildren select: [:each | each isSequence])
						detect: [ :sequence | sequence defines: variableName ]
						ifNone: [ nil ].

	modifiedNode ifNotNil: [ modifiedNode removeTemporaryNamed: variableName ].

	class compileTree: methodTree
]

{ #category : 'storing' }
RBRemoveTemporaryVariableTransformation >> storeOn: aStream [

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' variable: ''';
		nextPutAll: variableName;
		nextPutAll: ''' inMethod: ''';
		nextPutAll: selector;
		nextPutAll: ''' inClass: '.
	class storeOn: aStream.
	aStream nextPut: $)
]

{ #category : 'api' }
RBRemoveTemporaryVariableTransformation >> variable: aVariableName inMethod: aSelector inClass: aClass [

	self className: aClass.
	selector := aSelector.
	variableName := aVariableName
]

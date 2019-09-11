"
I am responsible for removing an assignment statement inside a method. In this implementation, the variable name and the expression value must be described separately.

Usage:
| transformation |
transformation := (RBRemoveAssignmentTransformation
				variable: 'variable'
				value: '1 asString'
				inMethod: #methodBefore
				inClass: #RBRemoveAssignmentTransformationTest)
				transform.
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
- there is an assignment to the given variable.

Observations about the transformation:
-  If the method has more than one assignment to the given variable, the transformation will remove the assignment that is more similar to the specified value, in terms of AST nodes.
"
Class {
	#name : #RBRemoveAssignmentTransformation,
	#superclass : #RBMethodTransformation,
	#instVars : [
		'variableName',
		'value'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #api }
RBRemoveAssignmentTransformation class >> model: aRBModel variable: aVariableName value: anExpression inMethod: aSelector inClass: aClassName [

	^ self new
		model: aRBModel;
		variable: aVariableName
		value: anExpression
		inMethod: aSelector
		inClass: aClassName;
		yourself
]

{ #category : #api }
RBRemoveAssignmentTransformation class >> variable: aVariableName value: anExpression inMethod: aSelector inClass: aClassName [

	^ self new
		variable: aVariableName
		value: anExpression
		inMethod: aSelector
		inClass: aClassName;
		yourself
]

{ #category : #preconditions }
RBRemoveAssignmentTransformation >> preconditions [

	^ (RBCondition
			withBlock: [ self definingClass isNotNil ]
			errorString: ('Class named <1s> does not exist' expandMacrosWith: className))
		& (RBCondition
			withBlock: [ self definingClass canUnderstand: selector ]
			errorString: ('Method named <1s> does not exist' expandMacrosWith: selector))
		& (RBCondition
			withBlock: [ (self definingMethod allChildren
				select: #isVariable thenCollect: #name)
				anySatisfy: [ :e | e asString = variableName ] ]
			errorString: ('Method named <1s> does not have an assignment for variable named <2s>'
				expandMacrosWith: selector with: variableName))
		& (RBCondition
			withBlock: [ (self definingMethod allChildren
				select: #isAssignment thenCollect: #variable)
				anySatisfy: [ :e | e name asString = variableName ] ]
			errorString: ('Method named <1s> does not have an assignment for variable named <2s>'
				expandMacrosWith: selector with: variableName))
]

{ #category : #executing }
RBRemoveAssignmentTransformation >> privateTransform [
	| methodTree valueNode assignments nodeToRemove |
	methodTree := self definingMethod.
	assignments := (methodTree allChildren select: #isAssignment)
		select: [ :each | each variable name asString = variableName ].
	nodeToRemove := assignments size = 1
		ifTrue: [ assignments first ]
		ifFalse: [ valueNode := self parserClass parseExpression: value.
			assignments
				detectMax: [ :node | 
					((node allChildren collect: #class)
						intersection: (valueNode allChildren collect: #class)) size ] ].
	nodeToRemove parent removeNode: nodeToRemove.
	class compileTree: methodTree
]

{ #category : #printing }
RBRemoveAssignmentTransformation >> storeOn: aStream [ 

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' variable: ''';
		nextPutAll: variableName;
		nextPutAll: ' value: ''';
		nextPutAll: value;
		nextPutAll: ' inMethod: ''';
		nextPutAll: selector;
		nextPutAll: ''' inClass: ''';
		nextPutAll: className.
	aStream nextPut: $)
]

{ #category : #api }
RBRemoveAssignmentTransformation >> variable: aVariableName value: anExpression inMethod: aSelector inClass: aClassName [

	self className: aClassName.
	selector := aSelector.
	variableName := aVariableName.
	value := anExpression
]

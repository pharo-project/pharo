"
I am responsible for adding an assignment statement (variable := anExpression) inside a method. Both variable and expression are passed as arguments separately.

Usage: 
| transformation |
transformation := (RBAddAssignmentTransformation
				variable: 'variable'
				value: '1 asString'
				inMethod: #methodBefore
				inClass: #RBAddAssignmentTransformationTest)
				transform.
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
- Checks whether there is a variable in this environment with the given name.

Observations about the transformation:
- If the variable is already assigned in this method, the transformation will add the new assignment just after it.
- Otherwise, the assignment will be positioned as the first statement of the method
"
Class {
	#name : #RBAddAssignmentTransformation,
	#superclass : #RBMethodTransformation,
	#instVars : [
		'variableName',
		'value'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #api }
RBAddAssignmentTransformation class >> model: aRBModel variable: aVariableName value: anExpression inMethod: aSelector inClass: aClassName [

	^ self new
		model: aRBModel;
		variable: aVariableName
		value: anExpression
		inMethod: aSelector
		inClass: aClassName;
		yourself
]

{ #category : #api }
RBAddAssignmentTransformation class >> variable: aVariableName value: anExpression inMethod: aSelector inClass: aClassName [

	^ self new
		variable: aVariableName
		value: anExpression
		inMethod: aSelector
		inClass: aClassName;
		yourself
]

{ #category : #preconditions }
RBAddAssignmentTransformation >> preconditions [

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
			errorString: ('Method named <1s> does not have a variable named <2s>'
				expandMacrosWith: selector with: variableName))
]

{ #category : #executing }
RBAddAssignmentTransformation >> privateTransform [
	"if the variable was previously assigned, the transformation adds
	 this assignment as the last one"

	| methodTree assignmentNode assignments previousNode |
	methodTree := self definingMethod.
	assignmentNode := RBAssignmentNode
		variable: (RBVariableNode named: variableName)
		value: (self parserClass parseExpression: value).

	"check whether there are some dependencies in the expression"
	assignments := (methodTree allChildren select: #isAssignment)
		select: [ :each | each variable name asString = variableName ].
	previousNode := assignments detectMax: #stop.
	previousNode
		ifNil: [ methodTree body addNodeFirst: assignmentNode ]
		ifNotNil: [ previousNode parent addNode: assignmentNode after: previousNode ].
	class compileTree: methodTree
]

{ #category : #printing }
RBAddAssignmentTransformation >> storeOn: aStream [ 

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
RBAddAssignmentTransformation >> variable: aVariableName value: anExpression inMethod: aSelector inClass: aClassName [

	self className: aClassName.
	selector := aSelector asSymbol.
	variableName := aVariableName asSymbol.
	value := anExpression
]

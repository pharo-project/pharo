"
I am a refactoring for changing a temporary variable to an instance variable.

The temporary variable is added to the class definition and removed from the temporary declaration in this method.

My preconditions verify that this variable is not yet used as an instance variable in the whole hierarchy of this class.
"
Class {
	#name : #RBTemporaryToInstanceVariableRefactoring,
	#superclass : #RBMethodRefactoring,
	#instVars : [
		'selector',
		'temporaryVariableName'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBTemporaryToInstanceVariableRefactoring class >> class: aClass selector: aSelector variable: aVariableName [ 
	^self new 
		class: aClass
		selector: aSelector
		variable: aVariableName
]

{ #category : #'instance creation' }
RBTemporaryToInstanceVariableRefactoring class >> model: aRBSmalltalk class: aClass selector: aSelector variable: aVariableName [ 
	^(self new)
		model: aRBSmalltalk;
		class: aClass
			selector: aSelector
			variable: aVariableName;
		yourself
]

{ #category : #preconditions }
RBTemporaryToInstanceVariableRefactoring >> checkForValidTemporaryVariable [
	| parseTree |
	parseTree := class parseTreeFor: selector.
	(parseTree allTemporaryVariables includes: temporaryVariableName) 
		ifFalse: 
			[self refactoringFailure: temporaryVariableName 
						, ' isn''t a valid temporary variable name'].
	(parseTree allArgumentVariables includes: temporaryVariableName) 
		ifTrue: 
			[self refactoringFailure: temporaryVariableName , ' is a block parameter'].
	(RBReadBeforeWrittenTester isVariable: temporaryVariableName
		readBeforeWrittenIn: parseTree) 
			ifTrue: 
				[self 
					refactoringWarning: ('<1s> is read before it is written.<n>Proceed anyway?' 
							expandMacrosWith: temporaryVariableName)]
]

{ #category : #initialization }
RBTemporaryToInstanceVariableRefactoring >> class: aClass selector: aSelector variable: aVariableName [ 
	class := self classObjectFor: aClass.
	selector := aSelector.
	temporaryVariableName := aVariableName
]

{ #category : #preconditions }
RBTemporaryToInstanceVariableRefactoring >> preconditions [
	^(RBCondition definesSelector: selector in: class) 
		& (RBCondition hierarchyOf: class
				definesVariable: temporaryVariableName asString) not 
			& (RBCondition withBlock: 
						[self checkForValidTemporaryVariable.
						true])
]

{ #category : #printing }
RBTemporaryToInstanceVariableRefactoring >> storeOn: aStream [ 
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' class: '.
	class storeOn: aStream.
	aStream
		nextPutAll: ' selector: #';
		nextPutAll: selector;
		nextPutAll: ' variable: ''';
		nextPutAll: temporaryVariableName;
		nextPut: $'.
	aStream nextPut: $)
]

{ #category : #transforming }
RBTemporaryToInstanceVariableRefactoring >> transform [

	| parseTree matcher method |

	method := class methodFor: selector.
	parseTree := method parseTree.
	parseTree ifNil: [ self refactoringFailure: 'Could not parse method' ].
	class addInstanceVariable: temporaryVariableName.
	( matcher := RBParseTreeRewriter removeTemporaryNamed: temporaryVariableName )
		executeTree: parseTree.
	method compileTree: matcher tree
]

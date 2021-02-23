"
I am a refactoring for creating a setUp method from a code fragment.

You can select an interval of some code in a test method and call this refactoring to create a setUp method implementing that code and replace the code by nothing. 
The selected class need to be a subclass of TestCase.

The preconditions are quite complex.
	- The code needs to be parseable valid code. 
	- The class must not implement setUp method.
	- Class must inherit from testCase class 
"
Class {
	#name : #RBExtractSetUpMethodRefactoring,
	#superclass : #RBExtractMethodRefactoring,
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #transforming }
RBExtractSetUpMethodRefactoring >> checkSingleAssignment: varName [ 
	((RBReadBeforeWrittenTester isVariable: varName
		readBeforeWrittenIn: extractedParseTree) 
			or: [extractedParseTree containsReturn]) 
			ifTrue: 
				[self 
					refactoringError: 'Cannot extract assignments to temporaries without all references'].
	class addInstanceVariable: varName.
	modifiedParseTree body removeTemporaryNamed: varName.
]

{ #category : #transforming }
RBExtractSetUpMethodRefactoring >> nameNewMethod: aSymbol [
	| args |
	args := parameters collect: [ :parm | RBVariableNode named: parm ].
	extractedParseTree renameSelector: aSymbol andArguments: args asArray.
	modifiedParseTree := RBParseTreeRewriter replace: self methodDelimiter with: '#callToSetUp' in: modifiedParseTree.
]

{ #category : #preconditions }
RBExtractSetUpMethodRefactoring >> preconditions [
	^(RBCondition definesSelector: selector in: class) &
	(RBCondition definesSelector: #setUp in: class) not &
	(RBCondition withBlock: [ class allSuperclasses anySatisfy: [ :e | e name = #TestCase ] ])
		& (RBCondition withBlock: 
					[self extractMethod.
					self checkSpecialExtractions.
					self checkReturn.
					needsReturn ifTrue: [extractedParseTree addReturn].
					self checkTemporaries.
					true])
]

{ #category : #transforming }
RBExtractSetUpMethodRefactoring >> transform [
	| existingSelector |
	existingSelector := self existingSelector.
	self nameNewMethod: #setUp.
	existingSelector ifNil: 
		[ self renameAllParameters.
		extractedParseTree body addNodeFirst: (RBParser parseExpression: 'super setUp').
		class compile: extractedParseTree newSource
			withAttributesFrom: (class methodFor: selector)].
	modifiedParseTree body removeNode: (modifiedParseTree body statements at: 1).
	class compileTree: modifiedParseTree
]

{ #category : #transforming }
RBExtractSetUpMethodRefactoring >> updateTemporariesInExtractedMethodFor: variableNames [
	"Remove temporaries in extract method"
	variableNames do: [ :varName | extractedParseTree body removeTemporaryNamed: varName ].
]

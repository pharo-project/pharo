Class {
	#name : 'RBInlineParameterParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'tests' }
RBInlineParameterParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBInlineParameterRefactoring };
		yourself
]

{ #category : 'accessing' }
RBInlineParameterParametrizedTest >> constructor [
	^ #inlineParameter:in:selector:
]

{ #category : 'failure tests' }
RBInlineParameterParametrizedTest >> testFailureInlineArgsWhitoutSameLiteral [

	self shouldFail: (self createRefactoringWithArguments: {
				 'aSymbol'.
				 RBClassDataForRefactoringTest.
				 ('inline' , 'Bar:') asSymbol })
]

{ #category : 'failure tests' }
RBInlineParameterParametrizedTest >> testFailureInlineBlockFailure [

	self shouldFail: (self createRefactoringWithArguments: {
				 'aBlock'.
				 RBClassDataForRefactoringTest.
				 ('inline' , 'Foo:') asSymbol })
]

{ #category : 'tests' }
RBInlineParameterParametrizedTest >> testInlineLiteralArray [
	| refactoring class |
	refactoring := self createRefactoringWithArguments:
		{ 'aSymbol' . RBClassDataForRefactoringTest . ('inline' , 'ParameterMethod:') asSymbol }.

	self proceedThroughWarning: [ self executeRefactoring: refactoring ].

	class := refactoring model classNamed: #RBClassDataForRefactoringTest.
	self assert: (class parseTreeForSelector: #inlineParameterMethod) equals: (self parseMethod: 'inlineParameterMethod | aSymbol | aSymbol := #(asdf). ^aSymbol isSymbol').
	self assert: (class parseTreeForSelector: #sendInlineParameterMethod) equals: (self parseMethod: 'sendInlineParameterMethod ^self inlineParameterMethod').
	self deny: (class directlyDefinesMethod: ('inline' , 'ParameterMethod:') asSymbol)
]

Class {
	#name : #RBInlineParameterParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBInlineParameterParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBInlineParameterRefactoring };
		yourself
]

{ #category : #'failure tests' }
RBInlineParameterParametrizedTest >> testInlineArgsWhitoutSameLiteral [
	self shouldFail: (rbClass 
		inlineParameter: 'aSymbol'
		in: RBRefactoryTestDataApp
		selector: ('inline' , 'Bar:') asSymbol)
]

{ #category : #'failure tests' }
RBInlineParameterParametrizedTest >> testInlineBlockFailure [
	self shouldFail: (rbClass 
		inlineParameter: 'aBlock'
		in: RBRefactoryTestDataApp
		selector: ('inline' , 'Foo:') asSymbol)
]

{ #category : #tests }
RBInlineParameterParametrizedTest >> testInlineLiteralArray [
	| refactoring class |
	refactoring := rbClass 
		inlineParameter: 'aSymbol'
		in: RBRefactoryTestDataApp
		selector: ('inline' , 'ParameterMethod:') asSymbol.
	
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self assert: (class parseTreeFor: #inlineParameterMethod) equals: (self parseMethod: 'inlineParameterMethod | aSymbol | aSymbol := #(asdf). ^aSymbol isSymbol').
	self assert: (class parseTreeFor: #sendInlineParameterMethod) equals: (self parseMethod: 'sendInlineParameterMethod ^self inlineParameterMethod').
	self deny: (class directlyDefinesMethod: ('inline' , 'ParameterMethod:') asSymbol)
]

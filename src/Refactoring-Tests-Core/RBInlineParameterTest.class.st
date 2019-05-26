Class {
	#name : #RBInlineParameterTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #'failure tests' }
RBInlineParameterTest >> testInlineBlockFailure [
	self shouldFail: (RBInlineParameterRefactoring 
			inlineParameter: 'aBlock'
			in: RBRefactoryTestDataApp
			selector: ('inline' , 'Foo:') asSymbol)
]

{ #category : #tests }
RBInlineParameterTest >> testInlineLiteralArray [
	| refactoring class |
	refactoring := RBInlineParameterRefactoring 
		inlineParameter: 'aSymbol'
		in: RBRefactoryTestDataApp
		selector: ('inline' , 'ParameterMethod:') asSymbol.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self assert: (class parseTreeFor: #inlineParameterMethod) = (self parseMethod: 'inlineParameterMethod | aSymbol | aSymbol := #(asdf). ^aSymbol isSymbol').
	self assert: (class parseTreeFor: #sendInlineParameterMethod) = (self parseMethod: 'sendInlineParameterMethod ^self inlineParameterMethod').
	self deny: (class directlyDefinesMethod: ('inline' , 'ParameterMethod:') asSymbol)
]

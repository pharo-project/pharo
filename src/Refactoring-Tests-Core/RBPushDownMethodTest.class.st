Class {
	#name : #RBPushDownMethodTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #tests }
RBPushDownMethodTest >> testPushDownMethod [
	| refactoring class |
	refactoring := RBPushDownMethodRefactoring 
		pushDown: #(#name: )
		from: RBLintRuleTestData.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBLintRuleTestData.
	self deny: (class directlyDefinesMethod: #name:).
	class subclasses do: 
		[ :each | 
		self assert: (each parseTreeFor: #name:) equals: (self parseMethod: 'name: aString name := aString') ]
]

{ #category : #'failure tests' }
RBPushDownMethodTest >> testPushDownMethodOnNonAbstractClass [
	| refactoring |
	refactoring := RBPushDownMethodRefactoring pushDown: #(#isArray) from: Array.
	self shouldFail: refactoring
]

{ #category : #tests }
RBPushDownMethodTest >> testPushDownMethodThatReferencesPoolDictionary [
	| refactoring class parseTree |
	parseTree := RBLintRuleTestData parseTreeFor: #junk.
	self proceedThroughWarning: 
		[ refactoring := RBPushDownMethodRefactoring 
			pushDown: #(#junk )
			from: RBLintRuleTestData.
		self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBLintRuleTestData.
	self deny: (class directlyDefinesMethod: #junk).
	class subclasses do: 
		[ :each | 
		self assert: (each parseTreeFor: #junk) equals: parseTree.
		self assert: (each directlyDefinesPoolDictionary: 'TextConstants' asSymbol) ]
]

{ #category : #'failure tests' }
RBPushDownMethodTest >> testPushDownNonExistantMenu [
	| refactoring |
	refactoring := RBPushDownMethodRefactoring 
		pushDown: #(#someMethodThatDoesNotExist )
		from: RBLintRuleTestData.
	self shouldFail: refactoring
]

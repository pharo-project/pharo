Class {
	#name : #RBRemoveAllSendersParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBRemoveAllSendersParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBRemoveAllSendersRefactoring };
		yourself
]

{ #category : #tests }
RBRemoveAllSendersParametrizedTest >> testRemoveMessageInsideBlock [
	| refactoring methodName |
	methodName := ('caller', '2') asSymbol.
	refactoring := rbClass  
			removeSendersOf: methodName.
	self setupInlineExpressionFor: refactoring toReturn: false.
	self proceedThroughWarning: [self executeRefactoring: refactoring ].
	"remove 2 senders"
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #multipleCalls) equals: (self parseMethod: 'multipleCalls').
	"it doesn't remove the sender, because its result is used"
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #inlineLast) equals: (self parseMethod: 'inlineLast
	5 = 3 ifTrue: [^self caller] ifFalse: [^self caller2]').
	
]

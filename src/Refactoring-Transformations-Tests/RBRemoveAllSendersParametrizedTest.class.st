Class {
	#name : 'RBRemoveAllSendersParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'accessing' }
RBRemoveAllSendersParametrizedTest >> constructor [
	^ #removeSendersOf:
]

{ #category : 'running' }
RBRemoveAllSendersParametrizedTest >> setUp [ 
	super setUp.
	rbClass := RBRemoveAllSendersRefactoring
]

{ #category : 'tests' }
RBRemoveAllSendersParametrizedTest >> testRemoveMessageInsideBlock [
	| refactoring methodName |
	methodName := #caller2.
	refactoring := self createRefactoringWithArguments: { methodName }.
	self setupInlineExpressionFor: refactoring toReturn: false.
	self proceedThroughWarning: [self executeRefactoring: refactoring ].
	"remove 2 senders"
	self assert: ((refactoring model classNamed: #RBClassDataForRefactoringTest) parseTreeForSelector: #multipleCalls) equals: (self parseMethod: 'multipleCalls').
	"it doesn't remove the sender, because its result is used"
	self assert: ((refactoring model classNamed: #RBClassDataForRefactoringTest) parseTreeForSelector: #inlineLast) equals: (self parseMethod: 'inlineLast
	5 = 3 ifTrue: [^self caller] ifFalse: [^self caller2]')
]

Class {
	#name : #RBInlineAllMethodTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #tests }
RBInlineAllMethodTest >> testInlineMethodWithMultipleSendersInMethod [
	| refactoring |
	refactoring := RBInlineAllSendersRefactoring 
		sendersOf: #caller2
		in: RBRefactoryTestDataApp.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #inlineLast) equals: (self parseMethod: 'inlineLast
	5 = 3 ifTrue: [^self caller] ifFalse: [^(1 to: 10) inject: 1 into: [:sum :each | sum * (self foo: each)]] ').
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #multipleCalls) equals: (self parseMethod: 'multipleCalls
	(1 to: 10) inject: 1 into: [:sum :each | sum * (self foo: each)].
	(1 to: 10) inject: 1 into: [:sum1 :each1 | sum1 * (self foo: each1)]')
]

{ #category : #tests }
RBInlineAllMethodTest >> testRecursiveMethod [
	| class |
	model := RBClassModelFactory rbNamespace new.
	class := model classNamed: self class name.
	class
		compile: 'foo ^self foo' classified: #(#accessing);
		compile: 'bar ^self foo' classified: #(#accessing).
	self executeRefactoring: (RBInlineAllSendersRefactoring 
				model: model
				sendersOf: #foo
				in: class).
	self assert: (class parseTreeFor: #foo) 
			equals: (self parseMethod: 'foo ^self foo').
	self assert: (class parseTreeFor: #bar) 
			equals: (self parseMethod: 'bar ^self foo')
]

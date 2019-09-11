Class {
	#name : #RBAddParameterTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #running }
RBAddParameterTest >> setUp [
	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #tests }
RBAddParameterTest >> testAddParameterForTwoArgumentMessage [
	| refactoring class |
	refactoring := RBAddParameterRefactoring 
		addParameterToMethod: ('called:' , 'on:') asSymbol
		in: RBRefactoryTestDataApp
		newSelector: #called:bar:on:
		initializer: '#(1.0)'.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self assert: (class parseTreeFor: #called:bar:on:) equals: (self parseMethod: 'called: anObject bar: aBlock on: anObject1
							Transcript
								show: anObject printString;
								cr.
								aBlock value').
	self assert: (class parseTreeFor: #caller) equals: (self parseMethod: 'caller
							| anObject |
							anObject := 5.
							self 
								called: anObject + 1
								bar: [^anObject] on: #(1.0)').
	self deny: (class directlyDefinesMethod: ('called:' , 'on:') asSymbol)
]

{ #category : #tests }
RBAddParameterTest >> testAddParameterThatReferencesGlobalAndLiteral [
	| refactoring class |
	refactoring := RBAddParameterRefactoring 
		addParameterToMethod: ('test' , 'Foo:') asSymbol
		in: RBRefactoryTestDataApp
		newSelector: #testFoo:bar:
		initializer: 'OrderedCollection new: 5'.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self assert: (class parseTreeFor: #testFoo:bar:) equals: (self parseMethod: 'testFoo: anObject bar: anObject1
								^self class + anObject').
	self assert: (class parseTreeFor: #callFoo) equals: (self parseMethod: 'callFoo ^self testFoo: 5 bar: (OrderedCollection new: 5)').
	self deny: (class directlyDefinesMethod: ('test' , 'Foo:') asSymbol)
]

{ #category : #tests }
RBAddParameterTest >> testAddParameterThatReferencesModelGlobal [
	| refactoring class |
	refactoring := RBAddParameterRefactoring 
		model: model
		addParameterToMethod: ('test' , 'Foo:') asSymbol
		in: RBRefactoryTestDataApp
		newSelector: #testFoo:bar:
		initializer: 'Bar new'.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self assert: (class parseTreeFor: #testFoo:bar:) equals: (self parseMethod: 'testFoo: anObject bar: anObject1
								^self class + anObject').
	self assert: (class parseTreeFor: #callFoo) equals: (self parseMethod: 'callFoo ^self testFoo: 5 bar: (Bar new)').
	self deny: (class directlyDefinesMethod: ('test' , 'Foo:') asSymbol)
]

{ #category : #tests }
RBAddParameterTest >> testAddParameterThatReferencesSelf [
	| refactoring class |
	refactoring := RBAddParameterRefactoring 
		addParameterToMethod: ('test' , 'Foo:') asSymbol
		in: RBRefactoryTestDataApp
		newSelector: #testFoo:bar:
		initializer: 'self printString'.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self assert: (class parseTreeFor: #testFoo:bar:) equals: (self parseMethod: 'testFoo: anObject bar: anObject1
								^self class + anObject').
	self assert: (class parseTreeFor: #callFoo) equals: (self parseMethod: 'callFoo ^self testFoo: 5 bar: (self printString)').
	self deny: (class directlyDefinesMethod: ('test' , 'Foo:') asSymbol)
]

{ #category : #'failure tests' }
RBAddParameterTest >> testBadInitializationCode [
	self
		shouldFail: (RBAddParameterRefactoring 
				addParameterToMethod: #name
				in: RBLintRuleTestData
				newSelector: #name:
				initializer: 'foo:');
		shouldFail: (RBAddParameterRefactoring 
				addParameterToMethod: #name
				in: RBLintRuleTestData
				newSelector: #name:
				initializer: 'foo')
]

{ #category : #'failure tests' }
RBAddParameterTest >> testModelBadInitializationCode [
	| refactoring |
	model removeClassNamed: #RBRefactoring.
	refactoring := RBAddParameterRefactoring 
		model: model
		addParameterToMethod: #name1
		in: RBLintRuleTestData
		newSelector: #name1:
		initializer: 'AddParameterRefactoring new'.
	self shouldFail: refactoring
]

{ #category : #'failure tests' }
RBAddParameterTest >> testModelNonExistantName [
	| refactoring |
	(model classNamed: #RBLintRuleTestData) removeMethod: #name.
	refactoring := RBAddParameterRefactoring 
		model: model
		addParameterToMethod: #name
		in: RBLintRuleTestData
		newSelector: #nameNew:
		initializer: 'nil'.
	self shouldFail: refactoring
]

{ #category : #'failure tests' }
RBAddParameterTest >> testNonExistantName [
	self shouldFail: (RBAddParameterRefactoring 
			addParameterToMethod: #name1
			in: RBLintRuleTestData
			newSelector: #name1:
			initializer: 'nil')
]

{ #category : #'failure tests' }
RBAddParameterTest >> testPrimitiveMethods [
	| refactoring |
	(model classNamed: #Object) 
		compile: 'foo <primitive: 100> ^#() primitiveFailed'
		classified: #(#accessing).
	refactoring := RBAddParameterRefactoring 
				model: model
				addParameterToMethod: #foo
				in: Object
				newSelector: #foo123124321s:
				initializer: '1'.
	self shouldFail: refactoring.
	refactoring := RBAddParameterRefactoring 
				addParameterToMethod: #at:
				in: Object
				newSelector: #at:foo:
				initializer: '1'.
	self shouldFail: refactoring.
	
]

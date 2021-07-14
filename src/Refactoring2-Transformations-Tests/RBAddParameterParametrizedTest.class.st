Class {
	#name : #RBAddParameterParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBAddParameterParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBAddParameterRefactoring };
		yourself
]

{ #category : #accessing }
RBAddParameterParametrizedTest >> defaultNamespace [ 
	^ RBNamespace
]

{ #category : #running }
RBAddParameterParametrizedTest >> setUp [
	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #tests }
RBAddParameterParametrizedTest >> testAddParameterAndRenameParameters [
	| oldSelector newSelector refactoring class newArg |
	oldSelector := ('called:' , 'on:') asSymbol.
	newSelector := #called:bar:on:.
	newArg := RBArgumentName name: 'anObject' value: '#(1.0)'.
	refactoring := rbClass 
		addParameterToMethod: oldSelector
		in: RBRefactoryTestDataApp
		newSelector: newSelector
		permutation: #(1 -1 2)
		newArgs: { newArg }.
	refactoring renameMap: (Array with: ((RBArgumentName name: 'aBlock') newName: 'aBlock1')).
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self assert: (class parseTreeFor: newSelector) equals: (self parseMethod: 'called: anObject bar: anObject1 on: aBlock1
							Transcript
								show: anObject printString;
								cr.
								aBlock1 value').
	self assert: (class parseTreeFor: #caller) equals: (self parseMethod: 'caller
							| anObject |
							anObject := 5.
							self 
								called: anObject + 1
								bar: #(1.0) on: [^anObject]').
	self deny: (class directlyDefinesMethod: oldSelector)
]

{ #category : #tests }
RBAddParameterParametrizedTest >> testAddParameterForTwoArgumentMessage [
	| oldSelector newSelector refactoring class newArg |
	oldSelector := ('called:' , 'on:') asSymbol.
	newSelector := #called:bar:on:.
	newArg := RBArgumentName name: 'anObject' value: '#(1.0)'.
	refactoring := rbClass 
		addParameterToMethod: oldSelector
		in: RBRefactoryTestDataApp
		newSelector: newSelector
		permutation: #(1 -1 2)
		newArgs: { newArg }.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self assert: (class parseTreeFor: newSelector) equals: (self parseMethod: 'called: anObject bar: anObject1 on: aBlock
							Transcript
								show: anObject printString;
								cr.
								aBlock value').
	self assert: (class parseTreeFor: #caller) equals: (self parseMethod: 'caller
							| anObject |
							anObject := 5.
							self 
								called: anObject + 1
								bar: #(1.0) on: [^anObject]').
	self deny: (class directlyDefinesMethod: oldSelector)
]

{ #category : #tests }
RBAddParameterParametrizedTest >> testAddParameterThatReferencesGlobalAndLiteral [

	| refactoring class oldSelector newSelector newArg |
	oldSelector := ('test' , 'Foo:') asSymbol.
	newSelector := #testFoo:bar:.
	newArg := RBArgumentName name: 'anObject' value: 'OrderedCollection new: 5'.
	refactoring := rbClass
		addParameterToMethod: oldSelector
		in: RBRefactoryTestDataApp
		newSelector: newSelector
		permutation: #(1 -1)
		newArgs: {newArg}.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self
		assert: (class parseTreeFor: newSelector)
		equals: (self parseMethod: 'testFoo: anObject bar: anObject1
								^self class + anObject').
	self
		assert: (class parseTreeFor: #callFoo)
		equals: (self parseMethod:
				 'callFoo ^self testFoo: 5 bar: (OrderedCollection new: 5)').
	self deny: (class directlyDefinesMethod: oldSelector)
]

{ #category : #tests }
RBAddParameterParametrizedTest >> testAddParameterThatReferencesModelGlobal [

	| refactoring class oldSelector newSelector newArg |
	oldSelector := ('test' , 'Foo:') asSymbol.
	newSelector := #testFoo:bar:.
	newArg := RBArgumentName name: 'anObject' value:  'Bar new'.
	refactoring := rbClass
		model: model
		addParameterToMethod: oldSelector
		in: RBRefactoryTestDataApp
		newSelector: newSelector
		permutation: #(1 -1)
		newArgs: {newArg}.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self
		assert: (class parseTreeFor: newSelector)
		equals: (self parseMethod: 'testFoo: anObject bar: anObject1
								^self class + anObject').
	self
		assert: (class parseTreeFor: #callFoo)
		equals:
		(self parseMethod: 'callFoo ^self testFoo: 5 bar: (Bar new)').
	self deny: (class directlyDefinesMethod: oldSelector)
]

{ #category : #tests }
RBAddParameterParametrizedTest >> testAddParameterThatReferencesSelf [

	| refactoring class oldSelector newSelector newArg |
	oldSelector := ('test' , 'Foo:') asSymbol.
	newSelector := #testFoo:bar:.
	newArg := RBArgumentName name: 'aFoo' value: 'self printString'. 
	refactoring := rbClass
		addParameterToMethod: oldSelector
		in: RBRefactoryTestDataApp
		newSelector: newSelector
		permutation: #(1 -1)
		newArgs: {newArg}.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self
		assert: (class parseTreeFor: newSelector)
		equals: (self parseMethod: 'testFoo: anObject bar: aFoo
								^self class + anObject').
	self
		assert: (class parseTreeFor: #callFoo)
		equals:
		(self parseMethod:
			 'callFoo ^self testFoo: 5 bar: (self printString)').
	self deny: (class directlyDefinesMethod: oldSelector)
]

{ #category : #tests }
RBAddParameterParametrizedTest >> testAddTwoParameters [

	| refactoring class oldSelector newSelector newArgs |
	oldSelector := ('test' , 'Foo:') asSymbol.
	newSelector := #testColl:foo:string:.
	newArgs := Array with: (RBArgumentName name: 'aColl' value: 'OrderedCollection new: 5')
		with: (RBArgumentName name: 'aString' value: '''string''').
	refactoring := rbClass
		addParameterToMethod: oldSelector
		in: RBRefactoryTestDataApp
		newSelector: newSelector
		permutation: #(-1 1 -2)
		newArgs: newArgs.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self
		assert: (class parseTreeFor: newSelector)
		equals: (self parseMethod: 'testColl: aColl foo: anObject string: aString
								^self class + anObject').
	self
		assert: (class parseTreeFor: #callFoo)
		equals: (self parseMethod:
				 'callFoo ^self testColl: (OrderedCollection new: 5) foo: 5 string: ''string''').
	self deny: (class directlyDefinesMethod: oldSelector)
]

{ #category : #'failure tests' }
RBAddParameterParametrizedTest >> testBadInitializationCode [
	| newArg |
	newArg := RBArgumentName name: 'anObject' value: 'foo:'.
	self
		shouldFail: (rbClass 
				addParameterToMethod: #name
				in: RBLintRuleTestData
				newSelector: #name:
				permutation: #(-1)
				newArgs: {newArg}).
	newArg := RBArgumentName name: 'anObject' value: 'foo'.
	self shouldFail: (rbClass 
				addParameterToMethod: #name
				in: RBLintRuleTestData
				newSelector: #name:
				permutation: #(-1)
				newArgs: {newArg}).
]

{ #category : #'failure tests' }
RBAddParameterParametrizedTest >> testInvalidNumArgsOfNewSelector [
	| newArg |
	newArg := RBArgumentName name: 'anObject' value: 'nil'.
	self shouldFail: (rbClass 
			addParameterToMethod: #checkSendersAccessTo:
			in: RBLintRuleTestData
			newSelector: #checkSendersAccessTo:
			permutation: #(1 -1)
			newArgs: {newArg})
]

{ #category : #'failure tests' }
RBAddParameterParametrizedTest >> testModelBadInitializationCode [
	| refactoring newArg |
	newArg := RBArgumentName name: 'anObject' value: 'AddParameterRefactoring new'.
	model removeClassNamed: #RBRefactoring.
	refactoring := rbClass 
		model: model
		addParameterToMethod: #name1
		in: RBLintRuleTestData
		newSelector: #name1:
		permutation: #(-1) 
		newArgs: {newArg}.
	self shouldFail: refactoring
]

{ #category : #'failure tests' }
RBAddParameterParametrizedTest >> testModelNonExistantName [
	| refactoring newArg |
	newArg := RBArgumentName name: 'anObject' value: 'nil'.
	(model classNamed: #RBLintRuleTestData) removeMethod: #name.
	refactoring := rbClass 
		model: model
		addParameterToMethod: #name
		in: RBLintRuleTestData
		newSelector: #nameNew:
		permutation: #(-1)
		newArgs: {newArg}.
	self shouldFail: refactoring
]

{ #category : #'failure tests' }
RBAddParameterParametrizedTest >> testNonExistantName [
	| newArg |
	newArg := RBArgumentName name: 'anObject' value: 'nil'.
	self shouldFail: (rbClass 
			addParameterToMethod: #name1
			in: RBLintRuleTestData
			newSelector: #name1:
			permutation: #(-1)
			newArgs: {newArg})
]

{ #category : #'failure tests' }
RBAddParameterParametrizedTest >> testPrimitiveMethods [
	| refactoring newArg |
	newArg := RBArgumentName name: 'anObject' value: '1'.
	(model classNamed: #Object) 
		compile: 'foo <primitive: 100> ^#() primitiveFailed'
		classified: #(#accessing).
	refactoring := rbClass 
				model: model
				addParameterToMethod: #foo
				in: Object
				newSelector: #foo123124321s:
				permutation: #(-1)
				newArgs: {newArg}.
	self shouldFail: refactoring.
	refactoring := rbClass 
				addParameterToMethod: #at:
				in: Object
				newSelector: #at:foo:
				permutation: #(1 -1)
				newArgs: {newArg}.
	self shouldFail: refactoring.
	
]

{ #category : #'failure tests' }
RBAddParameterParametrizedTest >> testUseExistingNewSelector [
	| newArg |
	newArg := RBArgumentName name: 'anObject' value: 'nil'.
	self shouldFail: (rbClass 
			addParameterToMethod: #checkSendersAccessTo:
			in: RBLintRuleTestData
			newSelector: #safeVariableNameFor:temporaries:
			permutation: #(1 -1)
			newArgs: {newArg})
]

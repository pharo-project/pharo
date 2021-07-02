Class {
	#name : #RBPushDownMethodTransformationTest,
	#superclass : #RBParametrizedTransformationTest,
	#instVars : [
		'selectors',
		'ownerClass',
		'hasPools',
		'pools'
	],
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBPushDownMethodTransformationTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #selectors ->  #(#name: ).
						#ownerClass -> #RBLintRuleTestData. 
						#hasPools -> false };
		addCase: { #selectors -> #(#junk ).
						#ownerClass -> #RBLintRuleTestData.
						#hasPools -> true.
						#pools -> { 'TextConstants' asSymbol } };
		yourself
]

{ #category : #accessing }
RBPushDownMethodTransformationTest >> hasPools [

	^ hasPools
]

{ #category : #accessing }
RBPushDownMethodTransformationTest >> hasPools: anObject [

	hasPools := anObject
]

{ #category : #accessing }
RBPushDownMethodTransformationTest >> ownerClass [

	^ ownerClass
]

{ #category : #accessing }
RBPushDownMethodTransformationTest >> ownerClass: anObject [

	ownerClass := anObject
]

{ #category : #accessing }
RBPushDownMethodTransformationTest >> pools [

	^ pools
]

{ #category : #accessing }
RBPushDownMethodTransformationTest >> pools: anObject [

	pools := anObject
]

{ #category : #accessing }
RBPushDownMethodTransformationTest >> selectors [

	^ selectors
]

{ #category : #accessing }
RBPushDownMethodTransformationTest >> selectors: anObject [

	selectors := anObject
]

{ #category : #'failure tests' }
RBPushDownMethodTransformationTest >> testPushDownMethodOnNonAbstractClass [
	| refactoring |
	refactoring := RBPushDownMethodTransformation pushDown: #(#isArray) from: Array.
	self shouldFail: refactoring asRefactoring
]

{ #category : #tests }
RBPushDownMethodTransformationTest >> testPushDownMethodRefactoring [
	| refactoring rbClass parseTrees |
	refactoring := RBPushDownMethodTransformation
		pushDown: selectors
		from: ownerClass.
	rbClass := refactoring model classNamed: ownerClass.
	parseTrees := Dictionary newFrom: (selectors collect: 
		[ :symbol | symbol -> (rbClass parseTreeFor: symbol) ]).
	refactoring asRefactoring transform.
	selectors do: [ :symbol | (rbClass directlyDefinesMethod: symbol) ].
	rbClass subclasses do: 
		[ :each | 
		selectors do: [ :symbol | 
			self assert: (each parseTreeFor: symbol) equals: (parseTrees at: symbol).
			hasPools ifTrue: [ pools do: 
				[ :pool | self assert: (rbClass definesPoolDictionary: pool) ] ]]]
]

{ #category : #'failure tests' }
RBPushDownMethodTransformationTest >> testPushDownMethodSubclassesReferToSelector [
	| refactoring |
	model 
		defineClass: 'Object subclass: #Superclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'Superclass subclass: #Foo1 instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'Superclass subclass: #Foo2 instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	(model classNamed: #Superclass) compile: 'yourself ^1' classified: #(#accessing).
	(model classNamed: #Foo1) compile: 'method1 ^super yourself' classified: #(#accessing).
	
	refactoring := RBPushDownMethodTransformation
		model: model
		pushDown: #(#yourself)
		from: (model classNamed: #Superclass).
	self shouldFail: refactoring asRefactoring
]

{ #category : #tests }
RBPushDownMethodTransformationTest >> testPushDownMethodTransformation [
	| refactoring rbClass parseTrees |
	refactoring := RBPushDownMethodTransformation
		pushDown: selectors
		from: ownerClass.
	rbClass := refactoring model classNamed: ownerClass.
	parseTrees := Dictionary newFrom: (selectors collect: 
		[ :symbol | symbol -> (rbClass parseTreeFor: symbol) ]).
	refactoring transform.
	selectors do: [ :symbol | (rbClass directlyDefinesMethod: symbol) ].
	rbClass subclasses do: 
		[ :each | 
		selectors do: [ :symbol | 
			self assert: (each parseTreeFor: symbol) equals: (parseTrees at: symbol) ]
		 ]
]

{ #category : #'failure tests' }
RBPushDownMethodTransformationTest >> testPushDownNonExistantMenu [
	| refactoring |
	refactoring := RBPushDownMethodTransformation
		pushDown: #(#someMethodThatDoesNotExist )
		from: RBLintRuleTestData.
	self shouldFail: refactoring asRefactoring
]

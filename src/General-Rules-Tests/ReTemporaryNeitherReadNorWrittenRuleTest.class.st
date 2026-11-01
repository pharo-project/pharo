Class {
	#name : 'ReTemporaryNeitherReadNorWrittenRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReTemporaryNeitherReadNorWrittenRuleTest >> testChangeRemovesOnlyItsTemporary [

	| critiques |
	self generateMethod: 'method | first second | ^ 1'.
	critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 2.

	(critiques detect: [ :each |
			 each sourceAnchor providesInterval and: [
				 ((self class >> #method) sourceCode copyFrom: each sourceAnchor interval first to: each sourceAnchor interval last) = 'first' ] ])
		change execute.

	self assert: (self class >> #method) ast allTemporaryVariables asArray equals: #( #second )
]

{ #category : 'tests' }
ReTemporaryNeitherReadNorWrittenRuleTest >> testChangeRemovesUnusedBlockTemporary [

	| critique |
	self generateMethod: 'method ^ [ | unused | 1 ] value'.
	critique := (self myCritiquesOnMethod: self class >> #method) first.

	critique change execute.

	self assertEmpty: (self class >> #method) ast allTemporaryVariables.
	self assert: (self perform: #method) equals: 1
]

{ #category : 'tests' }
ReTemporaryNeitherReadNorWrittenRuleTest >> testChangeRemovesUnusedTemporary [

	| critique |
	self generateMethod: 'method | unused used | used := 1. ^ used'.
	critique := (self myCritiquesOnMethod: self class >> #method) first.

	critique change execute.

	self assert: (self class >> #method) ast allTemporaryVariables asArray equals: #( #used ).
	self assert: (self perform: #method) equals: 1.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]

{ #category : 'tests' }
ReTemporaryNeitherReadNorWrittenRuleTest >> testChangeRemovesUnusedTemporaryOnClassSide [

	| critique |
	self generateMethod: 'method | unused used | used := 1. ^ used' classified: 'testing' in: self class class.
	critique := (self myCritiquesOnMethod: self class class >> #method) first.

	critique change execute.

	self assert: (self class class >> #method) ast allTemporaryVariables asArray equals: #( #used ).
	self assert: (self class perform: #method) equals: 1
]

{ #category : 'tests' }
ReTemporaryNeitherReadNorWrittenRuleTest >> testChangeRemovesUnusedTemporaryShadowingUsedOne [

	| critiques ast |
	self generateMethod: 'method
		| first second |
		first := 22. second := first + 1.
		first := [ | first | 33 ]. ^ 1'.
	critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1.

	critiques first change execute.

	ast := (self class >> #method) ast.
	self assert: (ast temporaries collect: [ :each | each name ]) asArray equals: #( #first #second ).
	self assertEmpty: ast blockNodes first temporaries.
	self assert: (self perform: #method) equals: 1.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]

{ #category : 'tests' }
ReTemporaryNeitherReadNorWrittenRuleTest >> testCritiqueOnUninstalledMethodProvidesNoChange [

	| ast critiques rule |
	ast := (RBParser parseMethod: 'methodNotInstalled |unused|')
		       doSemanticAnalysis;
		       yourself.
	self deny: (ast methodClass includesSelector: #methodNotInstalled).
	rule := ReTemporaryNeitherReadNorWrittenRule new.
	critiques := OrderedCollection new.
	ast nodesDo: [ :node | rule check: node forCritiquesDo: [ :critique | critiques add: critique ] ].

	self assert: critiques size equals: 1.
	self deny: critiques first providesChange
]

{ #category : 'tests' }
ReTemporaryNeitherReadNorWrittenRuleTest >> testCritiqueProvidesChange [

	| critiques |
	self generateMethod: 'method |unused|'.
	critiques := self myCritiquesOnMethod: self class >> #method.

	self assert: critiques size equals: 1.
	self assert: critiques first providesChange
]

{ #category : 'tests' }
ReTemporaryNeitherReadNorWrittenRuleTest >> testRule [

	self generateMethod: 'method |unused|'.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 1
]

{ #category : 'tests' }
ReTemporaryNeitherReadNorWrittenRuleTest >> testRuleNotViolated [

	self generateMethod: 'method |used| used := 1. ^used'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]

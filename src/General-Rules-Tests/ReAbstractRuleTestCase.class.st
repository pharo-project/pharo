"
I am the superclass of the tests of the Renraku rules. My subclasses are named after the rule they test followed by `Test`: `ReAsClassRuleTest` tests `ReAsClassRule`.

## Responsibility:

- Find the tested rule from the name of the test class (`subjectUnderTest`).
- Run the rule on a method or a class and answer its critiques, whatever the kind of entities the rule checks.
- Compile temporary methods and remove them after the test.

## Collaborators: 

- `ReAbstractRule`: the rule under test.
- `TestCase`: my superclass.

## Public API and Key Messages

- `myCritiquesOnMethod: aMethod` answers the critiques of the rule on a method. For a node rule, it runs the rule on each node of the method.
- `myCritiquesOnClass: aClass` answers the critiques of the rule on a class.
- `generateMethod:` (and `generateMethod:classified:in:`) compiles a method removed in `tearDown`.

Write in the test class a method that violates the rule and one that does not, and check them:

```
ReAsClassRuleTest >> methodWithAsClassIfAbsent
	self class asClassIfAbsent: [  ]

ReAsClassRuleTest >> testRuleWithAsClassIfAbsent
	self assert: (self myCritiquesOnMethod: self class >> #methodWithAsClassIfAbsent) size equals: 1

ReAsClassRuleTest >> testRuleNotViolated
	self assertEmpty: (self myCritiquesOnMethod: self class >> #testRuleNotViolated)
```

## Internal Representation

- `compiledMethods`: the methods compiled by `generateMethod:`, removed in `tearDown`.
"
Class {
	#name : 'ReAbstractRuleTestCase',
	#superclass : 'TestCase',
	#instVars : [
		'compiledMethods'
	],
	#category : 'General-Rules-Tests-Migrated',
	#package : 'General-Rules-Tests',
	#tag : 'Migrated'
}

{ #category : 'testing' }
ReAbstractRuleTestCase class >> isAbstract [

	^self == ReAbstractRuleTestCase
]

{ #category : 'utilities' }
ReAbstractRuleTestCase >> critiquesFor: ruleClass onMethod: method [
	| critiques |
	critiques := OrderedCollection new.
	ruleClass new
		check: method forCritiquesDo:[:critique | critiques add: critique].
	^critiques
]

{ #category : 'utilities' }
ReAbstractRuleTestCase >> generateMethod: code [ 

	^ self generateMethod: code classified: 'testing'
]

{ #category : 'utilities' }
ReAbstractRuleTestCase >> generateMethod: code classified: protocol [

    ^ self generateMethod: code classified: protocol in: self class
]

{ #category : 'utilities' }
ReAbstractRuleTestCase >> generateMethod: code classified: protocol in: aBehavior [

    | method |
    method := aBehavior >> (aBehavior compile: code classified: protocol).
    compiledMethods add: method.
    ^ method
]

{ #category : 'utilities' }
ReAbstractRuleTestCase >> myCritiques [

	^ self myCritiquesOnMethod: self class >> #sampleMethod
]

{ #category : 'utilities' }
ReAbstractRuleTestCase >> myCritiquesOnClass: aClass [
	| critiques |
	critiques := OrderedCollection new.
	self subjectUnderTest new
		check: aClass forCritiquesDo:[:critique | critiques add: critique].
	^critiques
]

{ #category : 'utilities' }
ReAbstractRuleTestCase >> myCritiquesOnMethod: method [

	| critiques rule |
	critiques := OrderedCollection new.
	rule := self subjectUnderTest new.
	self subjectUnderTest checksNode
		ifTrue: [
			| ast |
			ast := method ast.
			"for rewrite rules, we run every rule on a copy of the ast"
			rule isRewriteRule ifTrue: [ ast := ast copy ].
			ast nodesDo: [ :node |
				rule
					check: node
					forCritiquesDo: [ :critique | critiques add: critique ] ] ]
		ifFalse: [
			rule
				check: method
				forCritiquesDo: [ :critique | critiques add: critique ] ].
	^ critiques
]

{ #category : 'running' }
ReAbstractRuleTestCase >> setUp [
	super setUp.
	compiledMethods := OrderedCollection new
]

{ #category : 'utilities' }
ReAbstractRuleTestCase >> sourceAtCritique: critique [

	| anchor |
	anchor := critique sourceAnchor.
	^ anchor entity sourceCode atAll: anchor interval
]

{ #category : 'utilities' }
ReAbstractRuleTestCase >> subjectUnderTest [

	^ (self class name allButLast: 4) asClassInEnvironment:
		  self class environment
]

{ #category : 'running' }
ReAbstractRuleTestCase >> tearDown [

	compiledMethods do: [ :method | method removeFromSystem ].
	super tearDown
]

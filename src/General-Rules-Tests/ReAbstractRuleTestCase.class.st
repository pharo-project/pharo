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
ReAbstractRuleTestCase >> critiguesFor: ruleClass onMethod: method [
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

	| method |
	method := self class >> (self class compile: code classified: protocol).
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
ReAbstractRuleTestCase >> sourceAtChritique: critique [

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

Class {
	#name : 'ReDoNotSendStartingUpSuperInClassSideRuleKeyworkSelectorTest',
	#superclass : 'ParametrizedTestCase',
	#instVars : [
		'rule',
		'fixture',
		'compiledMethod'
	],
	#category : 'General-Rules-Tests-Potential Bugs',
	#package : 'General-Rules-Tests',
	#tag : 'Potential Bugs'
}

{ #category : 'building suites' }
ReDoNotSendStartingUpSuperInClassSideRuleKeyworkSelectorTest class >> testParameters [

    ^ ParametrizedTestMatrix new
          addCase:
              { (#fixture -> (ReDoNotSendStartingUpSuperInClassSideKeywordFixture onSelector: #startUp:)) };
          addCase:
              { (#fixture -> (ReDoNotSendStartingUpSuperInClassSideKeywordFixture onSelector: #shutDown:)) };
          addCase:
              { (#fixture -> (ReDoNotSendStartingUpSuperInClassSideUnaryFixture onSelector: #startUp)) };
          addCase:
              { (#fixture -> (ReDoNotSendStartingUpSuperInClassSideUnaryFixture onSelector: #shutDown)) };
          yourself
]

{ #category : 'running' }
ReDoNotSendStartingUpSuperInClassSideRuleKeyworkSelectorTest >> fixture: anObject [

    fixture := anObject
]

{ #category : 'utilities' }
ReDoNotSendStartingUpSuperInClassSideRuleKeyworkSelectorTest >> generateMethod: aString [

    ^ self generateMethod: aString classified: '-' in: self class class
]

{ #category : 'utilities' }
ReDoNotSendStartingUpSuperInClassSideRuleKeyworkSelectorTest >> generateMethod: code classified: protocol in: aBehavior [

    compiledMethod := aBehavior >> (aBehavior compile: code classified: protocol).
    ^ compiledMethod
]

{ #category : 'utilities' }
ReDoNotSendStartingUpSuperInClassSideRuleKeyworkSelectorTest >> generateMethod: aString in: aClass [

    ^ self generateMethod: aString classified: '-' in: aClass
]

{ #category : 'tests' }
ReDoNotSendStartingUpSuperInClassSideRuleKeyworkSelectorTest >> methodWithSuper [

    ^ fixture methodWithSuper
]

{ #category : 'tests' }
ReDoNotSendStartingUpSuperInClassSideRuleKeyworkSelectorTest >> methodWithoutSuper [

    ^ fixture methodWithoutSuper
]

{ #category : 'tests' }
ReDoNotSendStartingUpSuperInClassSideRuleKeyworkSelectorTest >> methodWithoutSuperWrongSelector [

    ^ fixture methodWithoutSuperWrongSelector
]

{ #category : 'utilities' }
ReDoNotSendStartingUpSuperInClassSideRuleKeyworkSelectorTest >> myCritiquesOnMethod: method [

    | critiques ast |
    critiques := OrderedCollection new.
    ast := method parseTree.
    ast nodesDo: [ :node | rule check: node forCritiquesDo: [ :critique | critiques add: critique ] ].
    ^ critiques
]

{ #category : 'running' }
ReDoNotSendStartingUpSuperInClassSideRuleKeyworkSelectorTest >> setUp [

    super setUp.
    rule := ReDoNotSendSuperShutDownInClassSideRule new
]

{ #category : 'running' }
ReDoNotSendStartingUpSuperInClassSideRuleKeyworkSelectorTest >> tearDown [

    compiledMethod removeFromSystem.
    super tearDown
]

{ #category : 'tests' }
ReDoNotSendStartingUpSuperInClassSideRuleKeyworkSelectorTest >> testDoesNotMatch [

    self generateMethod: self methodWithoutSuper.
    self assert: (self myCritiquesOnMethod: compiledMethod) size equals: 0
]

{ #category : 'tests' }
ReDoNotSendStartingUpSuperInClassSideRuleKeyworkSelectorTest >> testRuleMatchesOnce [

    self generateMethod: self methodWithSuper.
    self assert: (self myCritiquesOnMethod: compiledMethod) size equals: 1
]

{ #category : 'tests' }
ReDoNotSendStartingUpSuperInClassSideRuleKeyworkSelectorTest >> testShouldCheckMethod [

    self generateMethod: self methodWithSuper.
    self assert: (rule shouldCheckMethod: compiledMethod)
]

{ #category : 'tests' }
ReDoNotSendStartingUpSuperInClassSideRuleKeyworkSelectorTest >> testShouldNotCheckMethodInInstanceSide [

    self generateMethod: self methodWithoutSuper in: self class.
    self deny: (rule shouldCheckMethod: compiledMethod)
]

{ #category : 'tests' }
ReDoNotSendStartingUpSuperInClassSideRuleKeyworkSelectorTest >> testShouldNotCheckMethodWrongSelector [

    self generateMethod: self methodWithoutSuperWrongSelector.
    self deny: (rule shouldCheckMethod: compiledMethod)
]

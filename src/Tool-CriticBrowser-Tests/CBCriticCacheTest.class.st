Class {
	#name : #CBCriticCacheTest,
	#superclass : #TestCase,
	#instVars : [
		'criticCache'
	],
	#category : #'Tool-CriticBrowser-Tests-Tests'
}

{ #category : #running }
CBCriticCacheTest >> setUp [
	criticCache := MockCriticsCache new
]

{ #category : #adding }
CBCriticCacheTest >> testAddCritcForNewRuleShouldCreatAnIDentitySetAndAddCriticToIDSet [
	| mockRule mockCritic |
	mockRule := MockRuleUseForAddNumber1 new.
	mockCritic := MockCriticUseForAddNumber1 new.
	self deny: (criticCache critics includesKey: mockRule).
	criticCache addCritic: mockCritic forRule: mockRule.
	self assert: (criticCache critics at: mockRule) size equals: 1
]

{ #category : #adding }
CBCriticCacheTest >> testAddCritcForOldRuleShouldAddCriticToIDSet [
	| mockRule mockCritic mockCritic2 |
	mockRule := MockRuleUseForAddNumber1 new.
	mockCritic := MockCriticUseForAddNumber1 new.
	mockCritic2 := MockCriticUseForAddNumber2 new.
	criticCache addCritic: mockCritic forRule: mockRule.
	self assert: (criticCache critics at: mockRule) size equals: 1.
	criticCache addCritic: mockCritic2 forRule: mockRule.
	self assert: (criticCache critics at: mockRule) size equals: 2
]

{ #category : #adding }
CBCriticCacheTest >> testAddFalsePositiveForNewRuleShouldCreatAnIDentitySetAndAddCriticToIDSet [
	| mockRule mockCritic |
	mockRule := MockRuleUseForAddNumber1 new.
	mockCritic := MockCriticUseForAddNumber1 new.
	self deny: (criticCache falsePositives includesKey: mockRule).
	criticCache addFalsePositive: mockCritic forRule: mockRule.
	self assert: (criticCache falsePositives at: mockRule) size equals: 1
]

{ #category : #adding }
CBCriticCacheTest >> testAddFalsePositiveForOldRuleShouldAddCriticToIDSet [
	" oldRule mean that there is a key for this rulee yet"
	| mockRule mockCritic mockCritic2 |
	mockRule := MockRuleUseForAddNumber1 new.
	mockCritic := MockCriticUseForAddNumber1 new.
	mockCritic2 := MockCriticUseForAddNumber2 new.
	criticCache addFalsePositive: mockCritic forRule: mockRule.
	self assert: (criticCache falsePositives at: mockRule) size equals: 1.
	criticCache addFalsePositive: mockCritic2 forRule: mockRule.
	self assert: (criticCache falsePositives at: mockRule) size equals: 2
]

{ #category : #adding }
CBCriticCacheTest >> testAddFalsePositiveRuleForPackageShouldAdd3CriticToFalsePositives [
	| rule package|
	rule := GRInstanceVariableCapitalizationRule new.
	package := UseForTestCriticBrowser package.
	criticCache checker: ReSmalllintChecker new.
	criticCache checker
		runRules: rule asOrderedCollection
		onPackage: UseForTestCriticBrowser package
		withoutTestCase: false.
	criticCache critics
		at: rule
		put: (IdentitySet newFrom: (criticCache checker criticsOf: rule)).
	criticCache
		addFalsePositiveRule: rule
		forPackage: UseForTestCriticBrowser package.
	self assert: (criticCache falsePositives at: rule) size equals: 3.
	self assert: (criticCache falsePositiveRules includesKey: package).
]

{ #category : #initialiazeTest }
CBCriticCacheTest >> testInitialyseAllDictionaryShouldBeEmpty [
	self assert: criticCache critics isEmpty.
	self assert: criticCache falsePositives isEmpty.
	self assert: criticCache falsePositiveRules isEmpty.
	self assert: criticCache toDos isEmpty.
	self assert: criticCache falsePositiveClass isEmpty
]

{ #category : #adding }
CBCriticCacheTest >> testRemoveAnItemFromAllDictionary [
	| mockRule1 mockRule2 mockCritic1 mockCritic2 |
	mockRule1 := MockRuleUseForAddNumber1 title: 'mockRule1'.
	mockRule2 := MockRuleUseForAddNumber2 title: 'mockRule2'.
	mockCritic1 := MockCriticUseForAddNumber1 title: 'mockCritic1'.
	mockCritic2 := MockCriticUseForAddNumber2 title: 'mockCritic2'.
	criticCache addFalsePositive: mockCritic1 forRule: mockRule1.
	criticCache addToDo: mockCritic1 forRule: mockRule1.
	criticCache addCritic: mockCritic2 forRule: mockRule2.
	criticCache itemRemoved: MockCriticUseForAddNumber1.
	self
		assert: (criticCache falsePositives at: mockRule1)
		equals: {} asIdentitySet.
	self
		assert: (criticCache toDos at: mockRule1)
		equals: {} asIdentitySet.
	self
		assert: (criticCache critics at: mockRule2)
		equals: {mockCritic2} asIdentitySet
]

{ #category : #adding }
CBCriticCacheTest >> testRemoveCriticForRule [
	| mockRule1 mockRule2 mockCritic1 mockCritic2 |
	mockRule1 := MockRuleUseForAddNumber1 title: 'mockRule1'.
	mockRule2 := MockRuleUseForAddNumber2 title: 'mockRule2'.
	mockCritic1 := MockCriticUseForAddNumber1 title: 'mockCritic1'.
	mockCritic2 := MockCriticUseForAddNumber2 title: 'mockCritic2'.
	{mockCritic1.
	mockCritic2}
		do: [ :critic | criticCache addCritic: critic forRule: mockRule1 ].
	criticCache addCritic: mockCritic1 forRule: mockRule2.
	criticCache removeCritic: mockCritic1 forRule: mockRule1.
	self
		assert: (criticCache critics at: mockRule2)
		equals: {mockCritic1} asIdentitySet.
	self
		assert: (criticCache critics at: mockRule1)
		equals: {mockCritic2} asIdentitySet.
	criticCache removeCritic: mockCritic1 forRule: mockRule2.
	self
		assert: (criticCache critics at: mockRule2)
		equals: {} asIdentitySet
]

{ #category : #adding }
CBCriticCacheTest >> testRemoveFalsePositiveRuleForPackage [
	| mockRule2 mockRule1 mockPackage1 mockPackage2 |
	mockRule1 := MockRuleUseForAddNumber1 title: 'mockRule2'.
	mockRule2 := MockRuleUseForAddNumber2 title: 'mockRule1'.
	mockPackage1 := CBMockPackage title: 'mockPackage1'.
	mockPackage2 := CBMockPackage title: 'mockPackage2'.
	{mockRule1.
	mockRule2}
		do:
			[ :mockRule | criticCache addFalsePositiveRule: mockRule forPackage: mockPackage1 ].
	criticCache addFalsePositiveRule: mockRule1 forPackage: mockPackage2.
	criticCache
		removeFalsePositiveRule: mockRule1
		forPackage: mockPackage1.
	self
		assert: (criticCache falsePositiveRules at: mockPackage1)
		equals: {MockRuleUseForAddNumber2 uniqueIdentifierName} asIdentitySet.
	self
		assert: (criticCache falsePositiveRules at: mockPackage2)
		equals: {MockRuleUseForAddNumber1 uniqueIdentifierName} asIdentitySet.
	criticCache
		removeFalsePositiveRule: mockRule1
		forPackage: mockPackage2.
	self
		assert: (criticCache falsePositiveRules at: mockPackage2)
		equals: {} asIdentitySet
]

{ #category : #adding }
CBCriticCacheTest >> testRemoveFalsePositiveRuleForRule [
	| mockCritic1  mockCritic3 mockCritic4 mockRule1  |
	mockCritic1 := MockCriticUseForAddNumber1 title: 'mockCritic1'.
	mockCritic3 := MockCriticUseForAddNumber1 title: 'mockCritic3'.
	mockCritic4 := MockCriticUseForAddNumber1 title: 'mockCritic4'.
	mockRule1 := MockRuleUseForAddNumber1 title: 'mockRule1'.
	{ mockCritic1 . mockCritic3 . mockCritic4  } do: [:aCritic |criticCache addFalsePositive: aCritic  forRule: mockRule1   ].
	self assert: (criticCache falsePositives at: mockRule1) equals: { mockCritic1 . mockCritic3 . mockCritic4  } asIdentitySet.
	criticCache removeFalsePositive: mockCritic3  forRule: mockRule1.
	self deny: ((criticCache falsePositives at: mockRule1 ) includes: mockCritic3). 
]

{ #category : #adding }
CBCriticCacheTest >> testRemoveRule [
	| mockCritic1  mockCritic3 mockCritic4 mockRule1  mockCritic2 mockRule2 |
	mockCritic1 := MockCriticUseForAddNumber1 title: 'mockCritic1'.
	mockCritic2 := MockCriticUseForAddNumber1 title: 'mockCritic2'.
	mockCritic3 := MockCriticUseForAddNumber1 title: 'mockCritic3'.
	mockCritic4 := MockCriticUseForAddNumber1 title: 'mockCritic4'.
	mockRule1 := MockRuleUseForAddNumber1 title: 'mockRule1'.
	mockRule2 := MockRuleUseForAddNumber1 title: 'mockRule2'.
	{ mockCritic1 . mockCritic3 . mockCritic4  } do: [:aCritic |criticCache addFalsePositive: aCritic  forRule: mockRule1   ].
	{ mockCritic1 . mockCritic2 } do: [:aCritic | criticCache addToDo: aCritic  forRule:   mockRule1 ].
	{ mockCritic3 . mockCritic4  } do: [ :aCritic | criticCache addCritic: aCritic  forRule: mockRule1  ].
	{ mockCritic3 . mockCritic4  } do: [ :aCritic | criticCache addCritic: aCritic  forRule: mockRule2  ].
	criticCache removeRule: mockRule1.
	"should remove all entry mockRule1 in todo , falsePositive and critic dictionary"
	self assert: (criticCache critics includesKey: mockRule2).	  
	self deny: (criticCache toDos includesKey: mockRule1).
	self deny: (criticCache falsePositives includesKey: mockRule1).
	self deny: (criticCache critics includesKey: mockRule1).
	
]

{ #category : #adding }
CBCriticCacheTest >> testRemoveToDoForRule [
	| mockCritic1 mockCritic2 mockCritic3 mockCritic4 mockRule1 mockRule2 |
	mockCritic1 := MockCriticUseForAddNumber1 title: 'mockCritic1'.
	mockCritic2 := MockCriticUseForAddNumber1 title: 'mockCritic2'.
	mockCritic3 := MockCriticUseForAddNumber1 title: 'mockCritic3'.
	mockCritic4 := MockCriticUseForAddNumber1 title: 'mockCritic4'.
	mockRule1 := MockRuleUseForAddNumber1 title: 'mockRule1'.
	mockRule2 := MockRuleUseForAddNumber1 title: 'mockRule2'.
	{mockCritic1.
	mockCritic2.
	mockCritic3}
		do: [ :aCritic | criticCache addToDo: aCritic forRule: mockRule1 ].
	{mockCritic1.
	mockCritic2.
	mockCritic4}
		do: [ :aCritic | criticCache addToDo: aCritic forRule: mockRule2 ].
	self
		assert: (criticCache toDos at: mockRule2)
		equals:
			{mockCritic1.
			mockCritic2.
			mockCritic4} asIdentitySet.
	criticCache removeToDo: mockCritic4 forRule: mockRule2.
	self deny: ((criticCache toDos at: mockRule2) includes: mockCritic4)
]

{ #category : #adding }
CBCriticCacheTest >> testRemovedAClass [
	| rule package|
	rule := GRInstanceVariableCapitalizationRule new.
	package := UseForTestCriticBrowser package.
	criticCache checker: ReSmalllintChecker new.
	criticCache checker
		runRules: rule asOrderedCollection
		onPackage: UseForTestCriticBrowser package
		withoutTestCase: false.
	criticCache critics
		at: rule
		put: (IdentitySet newFrom: (criticCache checker criticsOf: rule)).
	criticCache itemRemoved: UseForTestCriticBrowser.
	self assert:  (criticCache critics at: rule) size equals: 1. 
]

{ #category : #adding }
CBCriticCacheTest >> testToDoForRule [
	| mockCritic mockRule |
	mockCritic := MockCriticUseForAddNumber1 new.
	mockRule := MockRuleUseForAddNumber1 new.
	self deny: (criticCache toDos includesKey: mockRule) .
	criticCache addToDo: mockCritic forRule: mockRule.
	self assert: (criticCache toDos at: mockRule) size equals: 1
]

Class {
	#name : #CBCriticsCacheTest,
	#superclass : #TestCase,
	#instVars : [
		'criticCache',
		'rule'
	],
	#category : #'Tool-CriticBrowser-Tests'
}

{ #category : #'instance creation' }
CBCriticsCacheTest >> newCritic [
	^ ReTrivialCritique withAnchor: (ReVarSearchSourceAnchor entity: UseForTestCriticBrowser string: #A) by: GRInstanceVariableCapitalizationRule new
]

{ #category : #running }
CBCriticsCacheTest >> setUp [
	super setUp.
	criticCache := CBCriticsCache new.
	rule := GRInstanceVariableCapitalizationRule new
]

{ #category : #adding }
CBCriticsCacheTest >> testAddCritcForNewRuleShouldCreatAnIDentitySetAndAddCriticToIDSet [
	| critic |
	critic := self newCritic.
	self deny: (criticCache critics includesKey: rule).
	criticCache addCritic: critic forRule: rule.
	self assert: (criticCache critics at: rule) size equals: 1
]

{ #category : #adding }
CBCriticsCacheTest >> testAddCritcForOldRuleShouldAddCriticToIDSet [
	| critic critic2 |
	critic := self newCritic.
	critic2 := self newCritic.
	criticCache addCritic: critic forRule: rule.
	self assert: (criticCache critics at: rule) size equals: 1.
	criticCache addCritic: critic2 forRule: rule.
	self assert: (criticCache critics at: rule) size equals: 2
]

{ #category : #adding }
CBCriticsCacheTest >> testAddFalsePositiveForNewRuleShouldCreatAnIDentitySetAndAddCriticToIDSet [
	| critic |
	critic := self newCritic.
	self deny: (criticCache falsePositives includesKey: rule).
	criticCache addFalsePositive: critic forRule: rule.
	self assert: (criticCache falsePositives at: rule) size equals: 1
]

{ #category : #adding }
CBCriticsCacheTest >> testAddFalsePositiveForOldRuleShouldAddCriticToIDSet [
	" oldRule mean that there is a key for this rulee yet"

	| critic critic2 |
	critic := self newCritic.
	critic2 := self newCritic.
	criticCache addFalsePositive: critic forRule: rule.
	self assert: (criticCache falsePositives at: rule) size equals: 1.
	criticCache addFalsePositive: critic2 forRule: rule.
	self assert: (criticCache falsePositives at: rule) size equals: 2
]

{ #category : #adding }
CBCriticsCacheTest >> testAddFalsePositiveRuleForPackageShouldAdd3CriticToFalsePositives [
	| package |
	package := UseForTestCriticBrowser package.
	criticCache checker: ReSmalllintChecker new.
	criticCache checker runRules: rule asOrderedCollection onPackage: UseForTestCriticBrowser package withoutTestCase: false.
	criticCache critics at: rule put: (IdentitySet newFrom: (criticCache checker criticsOf: rule)).
	criticCache addFalsePositiveRule: rule forPackage: UseForTestCriticBrowser package.
	self assert: (criticCache falsePositives at: rule) size equals: 3.
	self assert: (criticCache falsePositiveRules includesKey: package)
]

{ #category : #initialiazeTest }
CBCriticsCacheTest >> testInitialyseAllDictionaryShouldBeEmpty [
	self assertEmpty: criticCache critics.
	self assertEmpty: criticCache falsePositives.
	self assertEmpty: criticCache falsePositiveRules.
	self assertEmpty: criticCache toDos.
	self assertEmpty: criticCache falsePositiveClass
]

{ #category : #adding }
CBCriticsCacheTest >> testRemoveAnItemFromAllDictionary [
	| anotherRule critic critic2 |
	anotherRule := RBUnclassifiedMethodsRule new.
	critic := self newCritic.
	critic sourceAnchor initializeEnitity: self.
	critic2 := self newCritic.
	criticCache addFalsePositive: critic forRule: rule.
	criticCache addToDo: critic forRule: rule.
	criticCache addCritic: critic2 forRule: anotherRule.
	criticCache itemRemoved: self.
	self assert: (criticCache falsePositives at: rule) equals: {} asIdentitySet.
	self assert: (criticCache toDos at: rule) equals: {} asIdentitySet.
	self assert: (criticCache critics at: anotherRule) equals: {critic2} asIdentitySet
]

{ #category : #adding }
CBCriticsCacheTest >> testRemoveCriticForRule [
	| anotherRule critic critic2 |
	anotherRule := RBUnclassifiedMethodsRule new.
	critic := self newCritic.
	critic2 := self newCritic.
	{critic . critic2} do: [ :c | criticCache addCritic: c forRule: rule ].
	criticCache addCritic: critic forRule: anotherRule.
	criticCache removeCritic: critic forRule: rule.
	self assert: (criticCache critics at: anotherRule) equals: {critic} asIdentitySet.
	self assert: (criticCache critics at: rule) equals: {critic2} asIdentitySet.
	criticCache removeCritic: critic forRule: anotherRule.
	self assert: (criticCache critics at: anotherRule) equals: {} asIdentitySet
]

{ #category : #adding }
CBCriticsCacheTest >> testRemoveFalsePositiveRuleForPackage [
	| anotherRule package1 package2 |
	anotherRule := RBUnclassifiedMethodsRule new.
	package1 := self class package.
	package2 := CBCriticBrowser package.
	{rule . anotherRule} do: [ :r | criticCache addFalsePositiveRule: r forPackage: package1 ].
	criticCache addFalsePositiveRule: rule forPackage: package2.
	criticCache removeFalsePositiveRule: rule forPackage: package1.
	self assert: (criticCache falsePositiveRules at: package1) equals: {RBUnclassifiedMethodsRule uniqueIdentifierName} asIdentitySet.
	self assert: (criticCache falsePositiveRules at: package2) equals: {GRInstanceVariableCapitalizationRule uniqueIdentifierName} asIdentitySet.
	criticCache removeFalsePositiveRule: rule forPackage: package2.
	self assert: (criticCache falsePositiveRules at: package2) equals: {} asIdentitySet
]

{ #category : #adding }
CBCriticsCacheTest >> testRemoveFalsePositiveRuleForRule [
	| critic critic2 critic3 mockRule1 |
	critic := self newCritic.
	critic2 := self newCritic.
	critic3 := self newCritic.
	mockRule1 := GRInstanceVariableCapitalizationRule new.
	{critic . critic2 . critic3} do: [ :aCritic | criticCache addFalsePositive: aCritic forRule: mockRule1 ].
	self assert: (criticCache falsePositives at: mockRule1) equals: {critic . critic2 . critic3} asIdentitySet.
	criticCache removeFalsePositive: critic2 forRule: mockRule1.
	self deny: ((criticCache falsePositives at: mockRule1) includes: critic2)
]

{ #category : #adding }
CBCriticsCacheTest >> testRemoveRule [
	| critic critic2 critic3 critic4 anotherRule |
	critic := self newCritic.
	critic2 := self newCritic.
	critic3 := self newCritic.
	critic4 := self newCritic.
	anotherRule := GRInstanceVariableCapitalizationRule new.
	{critic . critic3 . critic4} do: [ :aCritic | criticCache addFalsePositive: aCritic forRule: rule ].
	{critic . critic2} do: [ :aCritic | criticCache addToDo: aCritic forRule: rule ].
	{critic3 . critic4} do: [ :aCritic | criticCache addCritic: aCritic forRule: rule ].
	{critic3 . critic4} do: [ :aCritic | criticCache addCritic: aCritic forRule: anotherRule ].
	criticCache removeRule: rule.
	self assert: (criticCache critics includesKey: anotherRule).
	self deny: (criticCache toDos includesKey: rule).
	self deny: (criticCache falsePositives includesKey: rule).
	self deny: (criticCache critics includesKey: rule)
]

{ #category : #adding }
CBCriticsCacheTest >> testRemoveToDoForRule [
	| critic critic2 critic3 critic4 anotherRule |
	critic := self newCritic.
	critic2 := self newCritic.
	critic3 := self newCritic.
	critic4 := self newCritic.
	anotherRule := GRInstanceVariableCapitalizationRule new.
	{critic . critic2 . critic3} do: [ :aCritic | criticCache addToDo: aCritic forRule: rule ].
	{critic . critic2 . critic4} do: [ :aCritic | criticCache addToDo: aCritic forRule: anotherRule ].
	self assert: (criticCache toDos at: anotherRule) equals: {critic . critic2 . critic4} asIdentitySet.
	criticCache removeToDo: critic4 forRule: anotherRule.
	self deny: ((criticCache toDos at: anotherRule) includes: critic4)
]

{ #category : #adding }
CBCriticsCacheTest >> testRemovedAClass [
	criticCache checker: ReSmalllintChecker new.
	criticCache checker runRules: rule asOrderedCollection onPackage: UseForTestCriticBrowser package withoutTestCase: false.
	criticCache critics at: rule put: (IdentitySet newFrom: (criticCache checker criticsOf: rule)).
	criticCache itemRemoved: UseForTestCriticBrowser.
	self assert: (criticCache critics at: rule) size equals: 1
]

{ #category : #adding }
CBCriticsCacheTest >> testToDoForRule [
	| critic |
	critic := self newCritic.
	self deny: (criticCache toDos includesKey: rule).
	criticCache addToDo: critic forRule: rule.
	self assert: (criticCache toDos at: rule) size equals: 1
]

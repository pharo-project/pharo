"
A CBRulesChooserPresenterTest is a test class for testing the behaivor of CBRulesChooserPresenter
"
Class {
	#name : #CBSelectRuleTest,
	#superclass : #TestCase,
	#instVars : [
		'rulesChooser',
		'rulesChooserWithItems',
		'rules',
		'rulesByGroup'
	],
	#category : #'Tool-CriticBrowser-Tests'
}

{ #category : #running }
CBSelectRuleTest >> setUp [

	super setUp.
	rulesChooser := CBSelectRule new.
	rules := (ReRuleManager visibleRuleClasses collect: [ :aClass | 
		          aClass new ]) sorted: [ :a :b | a name < b name ].
	rulesByGroup := (rules groupedBy: #group) associations collect: [ :as | 
		                CBCritiqueBrowserRulesGroup
			                named: as key
			                rules: as value ]
]

{ #category : #running }
CBSelectRuleTest >> tearDown [ 

	super tearDown.
]

{ #category : #tests }
CBSelectRuleTest >> testAddAll [

	rulesChooser items: rulesByGroup.
	rulesChooser addAllButton click.
	self assert: rulesChooser selectedRules equals: rules asSet
]

{ #category : #tests }
CBSelectRuleTest >> testAddSelectedComposite [

	| bugsGroup bugsRules |
	bugsGroup := rulesByGroup select: [ :item | item name = 'Bugs' ].
	bugsRules := bugsGroup first rules.
	rulesChooser items: rulesByGroup.
	rulesChooser list selectPath: (rulesChooser list pathIndexOf: bugsGroup).
	rulesChooser addButton click.
	self assert: rulesChooser selectedRules equals: bugsRules asSet 
]

{ #category : #tests }
CBSelectRuleTest >> testAddSelectedSingleRule [

	| bugsGroup bugsRule |
	bugsGroup := rulesByGroup select: [ :item | item name = 'Bugs' ].
	bugsRule := bugsGroup first rules first.
	rulesChooser items: rulesByGroup.
	rulesChooser list selectItem: bugsRule.
	rulesChooser addButton click.
	self
		assert: rulesChooser selectedRules
		equals: { bugsRule } asSet
]

{ #category : #tests }
CBSelectRuleTest >> testInitialize [

	| window |
	self assert: rulesChooser list roots isEmpty.
	self assert: rulesChooser selectedList roots isEmpty.
	window := rulesChooser openWithSpec.
	self assert: window isBuilt.
	window close
]

{ #category : #tests }
CBSelectRuleTest >> testOpen [

	| window |
	window := CBSelectRule open.
	self assert: window isBuilt.
	window close
]

{ #category : #tests }
CBSelectRuleTest >> testRemoveAll [

	rulesChooser items: rulesByGroup.
	rulesChooser selectedRules: rules.
	rulesChooser removeAllButton click.
	self assert: rulesChooser selectedRules isEmpty
]

{ #category : #tests }
CBSelectRuleTest >> testRemoveSelectedComposite [

	| styleGroup styleRules |
	rulesChooser := CBSelectRule new.
	styleGroup := rulesByGroup select: [ :item | item name = 'Style' ].
	styleRules := styleGroup first rules.
	rulesChooser items: rulesByGroup.
	rulesChooser selectedList selectPath:
 		(rulesChooser selectedList pathIndexOf: styleGroup).
	rulesChooser selectedRules: styleRules.
	rulesChooser removeButton click.
	self assert: rulesChooser selectedRules isEmpty
]

{ #category : #tests }
CBSelectRuleTest >> testRemoveSelectedRule [

	| apiGroup apiRule |
	rulesChooser := CBSelectRule new.
	apiGroup := rulesByGroup select: [ :item | item name = 'API Hints' ].
	apiRule := apiGroup first rules first.
	rulesChooser items: rulesByGroup.
	rulesChooser selectedRules: { apiRule } asSet.
	rulesChooser selectedList selection selectItem: apiRule.
	self
		assert: rulesChooser selectedRules
		equals: { apiRule } asSet.
	rulesChooser removeButton click.
	self assert: rulesChooser selectedRules isEmpty
]

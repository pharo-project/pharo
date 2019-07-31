"
I'm using to select rule for the criticBrowser*

    Instance Variables
	environment:		<Object>
	removeTestCase:		<Boolean>
	selectedRules:		<Set>
			use to store the selected rule

"
Class {
	#name : #CBSelectRuleBrowser,
	#superclass : #CBSelectBrowser,
	#instVars : [
		'environment',
		'removeTestCase',
		'selectedRules'
	],
	#category : #'Tool-CriticBrowser-Parts'
}

{ #category : #accessing }
CBSelectRuleBrowser class >> items [
	| rules rulesByGroup integrationRules |
	rules := (ReRuleManager visibleRuleClasses collect: #new) sorted: #name ascending.
	integrationRules := rules select: [ :r | PharoCriticRules pharoHardLintRules includes: r class ].
	rulesByGroup := (rules groupedBy: #group) associations collect: [ :as | CBRulesGroup named: as key rules: as value ].
	rulesByGroup sort: #name ascending.
	^ rulesByGroup copyWithFirst: (CBRulesGroup named: 'Pharo integration rules' rules: integrationRules)
]

{ #category : #'instance creation' }
CBSelectRuleBrowser class >> openWithEnvironment: anEnvironment removeTestCase: aBoolean [
	self new
		items: self items;
		environment: anEnvironment;
		removeTestCase: aBoolean;
		openWithSpec
]

{ #category : #private }
CBSelectRuleBrowser >> addAllItems [
	toSelectPresenter roots do: [ :group | selectedRules addAll: group rules ].
]

{ #category : #private }
CBSelectRuleBrowser >> addItems [
	toSelectPresenter selectedItem ifNotNil: [ :item | item isComposite ifFalse: [ selectedRules add: item ] ifTrue: [ selectedRules addAll: item rules ] ].
]

{ #category : #accessing }
CBSelectRuleBrowser >> environment: anEnvironment [
	environment := anEnvironment
]

{ #category : #initialization }
CBSelectRuleBrowser >> initialize [
	super initialize.
	selectedRules := Set new
]

{ #category : #initialization }
CBSelectRuleBrowser >> initializeWidgets [
	super initializeWidgets.
	toSelectPresenter := self newTreeTable.
	selectedPresenter := self newTreeTable.

	toSelectPresenter
		addColumn: (SpStringTableColumn evaluated: [ :eachItem | eachItem name ]);
		children: [ :rule | rule isComposite ifTrue: [ rule rules select: [ :r | (self isSelected: r) not or: [ r isComposite ] ] ] ifFalse: [ #() ] ].

	selectedPresenter
		addColumn: (SpStringTableColumn evaluated: [ :eachItem | eachItem name ]);
		children: [ :rule | rule isComposite ifTrue: [ rule rules select: [ :r | (self isSelected: r) or: [ r isComposite ] ] ] ifFalse: [ #() ] ]
]

{ #category : #initialization }
CBSelectRuleBrowser >> initializeWindow: aWindowPresenter [
	super initializeWindow: aWindowPresenter.
	aWindowPresenter title: 'Select rules'
]

{ #category : #private }
CBSelectRuleBrowser >> isSelected: aRule [
	"Check the rule identifier to not run twice the same rule"

	^ selectedRules anySatisfy: [ :rule | rule class uniqueIdentifierName = aRule class uniqueIdentifierName ]
]

{ #category : #accessing }
CBSelectRuleBrowser >> items: aCollection [
	toSelectPresenter roots: aCollection.
	selectedPresenter roots: aCollection
]

{ #category : #api }
CBSelectRuleBrowser >> nextAction [
	self withWindowDo: #close.

	CBCriticWorkingConfiguration current
		rules: selectedRules;
		environment: environment;
		removeTestCase: removeTestCase.

	CBCriticBrowser openOnWorkingConfiguration: CBCriticWorkingConfiguration current
]

{ #category : #private }
CBSelectRuleBrowser >> removeAllItems [
	selectedPresenter roots do: [ :item | selectedRules removeAllFoundIn: item ].
	self updateTrees
]

{ #category : #private }
CBSelectRuleBrowser >> removeItems [
	selectedPresenter selectedItem ifNotNil: [ :item | selectedRules removeAllFoundIn: (item isComposite ifFalse: [ {item} ] ifTrue: [ item rules ]) ].
	self updateTrees
]

{ #category : #accessing }
CBSelectRuleBrowser >> removeTestCase: aBoolean [
	removeTestCase := aBoolean
]

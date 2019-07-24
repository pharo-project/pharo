Class {
	#name : #SelectRuleBrowser,
	#superclass : #SelectBrowser,
	#instVars : [
		'environment',
		'removeTestCase',
		'selectedRules'
	],
	#category : #'Tool-CriticBrowser-Parts'
}

{ #category : #accessing }
SelectRuleBrowser class >> items [
	| rules rulesByGroup integrationRules |
	rules := (ReRuleManager visibleRuleClasses collect: #new) sorted: #name ascending.
	integrationRules := rules select: [ :r | PharoCriticRules pharoHardLintRules includes: r class ].
	rulesByGroup := (rules groupedBy: #group) associations collect: [ :as | CriticBrowserRulesGroup named: as key rules: as value ].
	rulesByGroup sort: #name ascending.
	^ rulesByGroup copyWithFirst: (CriticBrowserRulesGroup named: 'Pharo integration rules' rules: integrationRules)
]

{ #category : #'instance creation' }
SelectRuleBrowser class >> openWithEnvironment: anEnvironment removeTestCase: aBoolean [
	self new
		items: self items;
		environment: anEnvironment;
		removeTestCase: aBoolean;
		openWithSpec
]

{ #category : #private }
SelectRuleBrowser >> addAllItems [
	toSelectPresenter roots do: [ :group | selectedRules addAll: group rules ].
	self updateTrees
]

{ #category : #private }
SelectRuleBrowser >> addItems [
	toSelectPresenter selectedItem ifNotNil: [ :item | item isComposite ifFalse: [ selectedRules add: item ] ifTrue: [ selectedRules addAll: item rules ] ].
	self updateTrees
]

{ #category : #accessing }
SelectRuleBrowser >> environment: anEnvironment [
	environment := anEnvironment
]

{ #category : #initialization }
SelectRuleBrowser >> initialize [
	super initialize.
	selectedRules := Set new
]

{ #category : #initialization }
SelectRuleBrowser >> initializeWidgets [
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
SelectRuleBrowser >> initializeWindow: aWindowPresenter [
	super initializeWindow: aWindowPresenter.
	aWindowPresenter title: 'Select rules'
]

{ #category : #private }
SelectRuleBrowser >> isSelected: aRule [
	"Check the rule identifier to not run twice the same rule"

	^ selectedRules anySatisfy: [ :rule | rule class uniqueIdentifierName = aRule class uniqueIdentifierName ]
]

{ #category : #accessing }
SelectRuleBrowser >> items: aCollection [
	toSelectPresenter roots: aCollection.
	selectedPresenter roots: aCollection
]

{ #category : #api }
SelectRuleBrowser >> nextAction [
	self withWindowDo: #close.

	CriticWorkingConfiguration current
		rule: selectedRules;
		environment: environment;
		removeTestCase: removeTestCase.

	CBCriticBrowser openOnWorkingConfiguration: CriticWorkingConfiguration current
]

{ #category : #private }
SelectRuleBrowser >> removeAllItems [
	selectedPresenter roots do: [ :item | selectedRules removeAllFoundIn: item ].
	self updateTrees
]

{ #category : #private }
SelectRuleBrowser >> removeItems [
	selectedPresenter selectedItem ifNotNil: [ :item | selectedRules removeAllFoundIn: (item isComposite ifFalse: [ {item} ] ifTrue: [ item rules ]) ].
	self updateTrees
]

{ #category : #accessing }
SelectRuleBrowser >> removeTestCase: aBoolean [
	removeTestCase := aBoolean
]

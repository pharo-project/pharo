"
To select rules.
"
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

{ #category : #'instance creation' }
SelectRuleBrowser class >> items [

	| rules rulesByGroup integrationRules |
	rules := (ReRuleManager visibleRuleClasses collect: #new) sorted: [ :a :b |
		a name < b name ].
	integrationRules := rules select: [ :r |
		PharoCriticRules pharoHardLintRules includes: r class ].
	rulesByGroup := ((rules groupedBy: #group) associations collect: [ :as |
		CriticBrowserRulesGroup named: as key rules: as value ]).
	rulesByGroup sort: [ :a :b | a name < b name ].
	^ rulesByGroup copyWithFirst: (
		CriticBrowserRulesGroup
			named: 'Pharo integration rules'
			rules: integrationRules)
]

{ #category : #'instance creation' }
SelectRuleBrowser class >> openWithEnvironment: anEnvironment removeTestCase: aBoolean [

	self new
		items: self items;
		environment: anEnvironment;
		removeTestCase: aBoolean;
		openWithSpec;
		updateTree
]

{ #category : #specs }
SelectRuleBrowser class >> title [
	^ 'Select rules'
]

{ #category : #private }
SelectRuleBrowser >> addAllItems [
	
	toSelectPresenter roots do: [ :group | selectedRules addAll: group rules].
	self updateTree  	  
]

{ #category : #private }
SelectRuleBrowser >> addItems [
	toSelectPresenter selectedItem
		ifNotNil:
			[ :item |
			item content isComposite
				ifFalse: [ selectedRules add: item content ]
				ifTrue: [ selectedRules addAll: item content rules ] ].
	self updateTree
]

{ #category : #private }
SelectRuleBrowser >> defaultRulesRootName [
	^ 'Selected Rules'
]

{ #category : #accessing }
SelectRuleBrowser >> environment: aEnv [
	environment := aEnv
]

{ #category : #initialization }
SelectRuleBrowser >> initialize [
	| selectedRulesChildrenBlock rulesToSelectChildrenBlock |
	super initialize.
	toSelectPresenter := self newTreeTable.
	selectedPresenter := self newTreeTable.
	toSelectPresenter
		addColumn: (SpStringTableColumn evaluated: [ :eachItem | eachItem name ]).
	selectedPresenter
		addColumn: (SpStringTableColumn evaluated: [ :eachItem | eachItem name ]).
	selectedRules := Set new.
	selectedRulesChildrenBlock := [ :rule | 
	rule isComposite
		ifTrue: [ rule rules
				select: [ :r | (self isSelected: r) or: [ r isComposite ] ] ]
		ifFalse: [ #() ] ].
	rulesToSelectChildrenBlock := [ :rule | 
	rule isComposite
		ifTrue: [ rule rules
				select: [ :r | (self isSelected: r) not or: [ r isComposite ] ] ]
		ifFalse: [ #() ] ].
	toSelectPresenter children: rulesToSelectChildrenBlock.
	selectedPresenter children: selectedRulesChildrenBlock
]

{ #category : #private }
SelectRuleBrowser >> isSelected: aRule [
	"Check the rule identifier to not run twice the same rule"

	^ selectedRules
		anySatisfy: [ :rule | rule class uniqueIdentifierName = aRule class uniqueIdentifierName ]
]

{ #category : #accessing }
SelectRuleBrowser >> items: aCollection [
	
	toSelectPresenter roots: aCollection.
	selectedPresenter roots: aCollection

]

{ #category : #api }
SelectRuleBrowser >> nextAction [

	self delete.
	
	CriticWorkingConfiguration current
		rule: selectedRules;
		environment: environment;
		removeTestCase: removeTestCase.
	CBCriticBrowser openOnWorkingConfiguration: CriticWorkingConfiguration current
]

{ #category : #api }
SelectRuleBrowser >> removeAllItems [
	
	selectedPresenter roots do: [ :item |  
		selectedRules removeAllFoundIn: item value ].
	self updateTree 	  
]

{ #category : #api }
SelectRuleBrowser >> removeItems [
	
	selectedPresenter selectedItem
		ifNotNil: [ :item | selectedRules removeAllFoundIn: item content leaves ].
	self updateTree  	  
]

{ #category : #accessing }
SelectRuleBrowser >> removeTestCase: aBoolean [
	removeTestCase := aBoolean
]

{ #category : #api }
SelectRuleBrowser >> updateTree [

	toSelectPresenter needRebuild: false.
	toSelectPresenter buildWithSpec.
	selectedPresenter needRebuild: false.
	selectedPresenter buildWithSpec
]

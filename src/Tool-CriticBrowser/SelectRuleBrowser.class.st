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
	
	toSelectModel roots do: [ :group | selectedRules addAll: group rules].
	self updateTree  	  
]

{ #category : #private }
SelectRuleBrowser >> addItems [
	toSelectModel selectedItem
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
	| wrappingBlock selectedRulesChildrenBlock rulesToSelectChildrenBlock |
	super initialize.
	toSelectModel := self instantiate: TreePresenter.
	selectedModel := self instantiate: TreePresenter.
	selectedRules := Set new.
	selectedRulesChildrenBlock := [ :rule | 
		rule isComposite
			ifTrue: [ rule rules
				select: [:r | (self isSelected: r) or: [ r isComposite ]]]
			ifFalse: [ #() ]].
	rulesToSelectChildrenBlock := [ :rule | 
		rule isComposite
			ifTrue: [ rule rules
				select: [:r | (self isSelected: r) not or: [ r isComposite ]]]
			ifFalse: [ #() ]].
	toSelectModel childrenBlock: rulesToSelectChildrenBlock.
	selectedModel childrenBlock: selectedRulesChildrenBlock.
	wrappingBlock := [:rule | rule name ].
	toSelectModel displayBlock: wrappingBlock.
	selectedModel displayBlock: wrappingBlock
]

{ #category : #private }
SelectRuleBrowser >> isSelected: aRule [
	"Check the rule identifier to not run twice the same rule"

	^ selectedRules
		anySatisfy: [ :rule | rule class uniqueIdentifierName = aRule class uniqueIdentifierName ]
]

{ #category : #accessing }
SelectRuleBrowser >> items: aCollection [
	
	toSelectModel roots: aCollection.
	selectedModel roots: aCollection

]

{ #category : #api }
SelectRuleBrowser >> nextAction [

	self delete.
	
	CriticWorkingConfiguration current
		rule: selectedRules;
		environment: environment;
		removeTestCase: removeTestCase.
	CriticBrowser openOnWorkingConfiguration: CriticWorkingConfiguration current
]

{ #category : #api }
SelectRuleBrowser >> removeAllItems [
	
	selectedModel roots do: [ :item |  
		selectedRules removeAllFoundIn: item value ].
	self updateTree 	  
]

{ #category : #api }
SelectRuleBrowser >> removeItems [
	
	selectedModel selectedItem
		ifNotNil: [ :item | selectedRules removeAllFoundIn: item content leaves ].
	self updateTree  	  
]

{ #category : #accessing }
SelectRuleBrowser >> removeTestCase: aBoolean [
	removeTestCase := aBoolean
]

{ #category : #api }
SelectRuleBrowser >> updateTree [

	toSelectModel updateTree.
	selectedModel updateTree
]

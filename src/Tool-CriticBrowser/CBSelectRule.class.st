"
I am a presenter that allows to select items from a list of rules organized by group in my left column and put that items on a selected list in my rigth column. I support selection for SpTreeTablePresenter. 

If you click on '>', I can add a group rules or a singe rule to selected list.
If you click on '<', I can remove a group rules or a singe rule from selected list.

If you click on 'Next', I will open the CBCritiqueBrowser.
I send selected rules from selected list to CBCritiqueBrowser. 

You can open me whit:
`CBSelectRule open`

"
Class {
	#name : #CBSelectRule,
	#superclass : #SpPresenter,
	#instVars : [
		'selectedList',
		'selectedItems',
		'list',
		'addAllButton',
		'methodName',
		'addButton',
		'removeAllButton',
		'buttonBar',
		'removeButton',
		'items',
		'nextButton',
		'selectedRules',
		'environment',
		'removeTestCase'
	],
	#category : #'Tool-CriticBrowser-Applier'
}

{ #category : #'instance creation' }
CBSelectRule class >> items [

	| rules rulesByGroup |
	rules := (ReRuleManager visibleRuleClasses collect: [ :aClass | 
		          aClass new ]) sorted: [ :a :b | a name < b name ].
	rulesByGroup := (rules groupedBy: #group) associations collect: [ :as | 
		                CBCritiqueBrowserRulesGroup
			                named: as key
			                rules: as value ].
	rulesByGroup sort: [ :a :b | a name < b name ].
	^ rulesByGroup
]

{ #category : #'instance creation' }
CBSelectRule class >> open [

	<example>
	| packages env |
	packages := RPackageOrganizer default packages.
	env := RBPackageEnvironment new packages: packages.
	^ (self openWithEnvironment: env removeTestCase: false)
]

{ #category : #'instance creation' }
CBSelectRule class >> openWithEnvironment: anEnvironment removeTestCase: aBoolean [

	^ self new
		items: self items;
		environment: anEnvironment;
		removeTestCase: aBoolean;
		updateTree;
		openWithSpec
		
]

{ #category : #adding }
CBSelectRule >> addAll [

	selectedList roots do: [ :group | selectedRules addAll: group rules ].
	self update.
	nextButton enabled: selectedRules isNotEmpty.
]

{ #category : #adding }
CBSelectRule >> addAllButton [

	^ addAllButton
]

{ #category : #adding }
CBSelectRule >> addButton [

	^ addButton
]

{ #category : #adding }
CBSelectRule >> addSelected [

	list selectedItem ifNotNil: [ :item | 
		item isComposite
			ifFalse: [ selectedRules add: item ]
			ifTrue: [ selectedRules addAll: item rules ] ].
	self update.
	nextButton enabled: selectedRules isNotEmpty.
]

{ #category : #adding }
CBSelectRule >> environment: aEnviroment [

	environment := aEnviroment
]

{ #category : #adding }
CBSelectRule >> groupRules [
	^ #(
	#'API Change'
	#'API Hints'
	#'Architectural'
	#'Bugs'
	#'Coding Idiom Violation'
	#'Design Flaws'
	#'Optimization'
	#'Potential Bugs'
	#'Rubric'
	#'SUnit'
	#'Style'
	#'Unclassified rules'  )
]

{ #category : #adding }
CBSelectRule >> initalizeSelectedList [

	selectedList := self instantiate: SpTreePresenter
]

{ #category : #adding }
CBSelectRule >> initialize [

	| wrappingBlock selectedRulesChildrenBlock rulesToSelectChildrenBlock |
	super initialize.
	self initializeList.
	self initalizeSelectedList.
	selectedRules := Set new.
	selectedRulesChildrenBlock := [ :rule | 
	                              rule isComposite
		                              ifTrue: [ 
			                              rule rules select: [ :r | 
				                              (self isSelected: r) or: [ 
					                              r isComposite ] ] ]
		                              ifFalse: [ #(  ) ] ].
	rulesToSelectChildrenBlock := [ :rule | 
	                              rule isComposite
		                              ifTrue: [ 
			                              rule rules select: [ :r | 
				                              (self isSelected: r) not or: [ 
					                              r isComposite ] ] ]
		                              ifFalse: [ #(  ) ] ].
	list children: rulesToSelectChildrenBlock.
	selectedList children: selectedRulesChildrenBlock.
	wrappingBlock := [ :rule | rule name ].
	list display: wrappingBlock.
	selectedList display: wrappingBlock
]

{ #category : #adding }
CBSelectRule >> initializeButtonBar [

	buttonBar 
		add: self addButton 
			expand: false;
		add: self addAllButton
			expand: false;
		add: self removeButton
			expand: false;
		add: self removeAllButton
			expand: false
]

{ #category : #adding }
CBSelectRule >> initializeButtons [

	addButton := self newButton
				addStyle: 'small';
				label: '>';
				action: [ self addSelected ];
				yourself.

	addAllButton := self newButton
				addStyle: 'small';
				label: '>>';
				action: [ self addAll ];
				yourself.

	removeButton := self newButton
				addStyle: 'small';
				label: '<';
				action: [ self removeSelected ];
				yourself.

	removeAllButton := self newButton
				addStyle: 'small';
				label: '<<';
				action: [ self removeAll ];
				yourself
]

{ #category : #adding }
CBSelectRule >> initializeLayout [

	self layout: (SpBoxLayout newTopToBottom
			 add: (SpBoxLayout newTopToBottom
					  spacing: 5;
					  add: (SpPanedLayout newLeftToRight
							   add: (SpBoxLayout newLeftToRight
									    spacing: 5;
									    add: #list;
									    add: (buttonBar := SpBoxLayout newTopToBottom)
									    expand: false;
									    yourself);
							   add: #selectedList;
							   yourself);
					  yourself);
			 add: (SpBoxLayout newLeftToRight
					  addLast: #nextButton;
					  yourself)
			 height: 25;
			 yourself)
]

{ #category : #adding }
CBSelectRule >> initializeList [

	list := self instantiate: SpTreePresenter.
	removeButton := self newButton
				addStyle: 'small';
				label: '<';
				action: [ self removeSelected ];
				yourself.

	removeAllButton := self newButton
				addStyle: 'small';
				label: '<<';
				action: [ self removeAll ];
				yourself
]

{ #category : #adding }
CBSelectRule >> initializeNextButton [

	nextButton := self newButton.
	nextButton
		icon: (self iconNamed: #smallOk);
		label: 'Next';
		enabled: false;
		action: [ self nextAction ]
]

{ #category : #adding }
CBSelectRule >> initializePresenters [

	self initializeLayout.
	self initializeList.
	self initalizeSelectedList.
	self initializeButtons.
	self initializeButtonBar.
	self initializeNextButton
]

{ #category : #adding }
CBSelectRule >> initializeWindow: aWindowPresenter [

	aWindowPresenter 
		title: self title;
		initialExtent: 450@360
]

{ #category : #adding }
CBSelectRule >> isSelected: aRule [

	"Check the rule identifier to not run twice the same rule"

	^ selectedRules anySatisfy: [ :rule | 
		  rule class uniqueIdentifierName = aRule class uniqueIdentifierName ]
]

{ #category : #adding }
CBSelectRule >> items: aCollection [

	list roots: aCollection.
	selectedList roots: aCollection
]

{ #category : #adding }
CBSelectRule >> list [
	
	^ list
]

{ #category : #adding }
CBSelectRule >> list: anObject [

	list := anObject
]

{ #category : #adding }
CBSelectRule >> methodName [

	^ methodName
]

{ #category : #adding }
CBSelectRule >> nextAction [

	self delete.

	CBCritiqueWorkingConfiguration current
		rule: selectedRules;
		environment: environment;
		removeTestCase: removeTestCase.
	CBCritiqueBrowser openOnWorkingConfiguration:
		CBCritiqueWorkingConfiguration current
]

{ #category : #adding }
CBSelectRule >> removeAll [

	selectedList roots do: [ :item | 
		selectedRules removeAllFoundIn: item value ].
	self update.
	nextButton enabled: selectedRules isNotEmpty
]

{ #category : #adding }
CBSelectRule >> removeAllButton [

	^ removeAllButton
]

{ #category : #adding }
CBSelectRule >> removeButton [

	^ removeButton
]

{ #category : #adding }
CBSelectRule >> removeSelected [

	selectedList selectedItem ifNotNil: [ :item | 
		(self groupRules includes: item name)
			ifTrue: [ selectedRules removeAll: item rules ]
			ifFalse: [ selectedRules remove: item ] ].
	self update.
	nextButton enabled: selectedRules isNotEmpty
]

{ #category : #adding }
CBSelectRule >> removeTestCase: aBoolean [

	removeTestCase := aBoolean
]

{ #category : #adding }
CBSelectRule >> selectedItems [

	^ selectedItems ifNil: [ selectedItems := #() ]
]

{ #category : #adding }
CBSelectRule >> selectedList [

	^ selectedList
]

{ #category : #adding }
CBSelectRule >> selectedList: anObject [

	selectedList := anObject
]

{ #category : #adding }
CBSelectRule >> selectedRules [

	^ selectedRules
]

{ #category : #adding }
CBSelectRule >> selectedRules: anObject [

	selectedRules := anObject
]

{ #category : #adding }
CBSelectRule >> setModelBeforeInitialization: aCollection [

	items := aCollection first.
	selectedItems := aCollection second.
	methodName := aCollection third
]

{ #category : #adding }
CBSelectRule >> title [

	^ 'Select rules'
]

{ #category : #adding }
CBSelectRule >> updateTree [

	list update.
	selectedList update
]

"
i'm the CriticBrowser 

with me you can analyse some class to see if it break some rule

HOW TO RUN
	
	| env rules |
	someRules := RBCompositeLintRule allGoodRules rules.
	env := RBBrowserEnvironment default.
	CBCriticBrowser openOnRule: someRules onEnvironment: env

LAYOUT 

|--------------------------------------|
|	#rulesPresenter ||#criticsPresenter |
|--------------------------------------|
|  all buttons in TAG button           |
|--------------------------------------|
|  comment rule \ | critic location \  |
| selected tab 	                        |
---------------------------------------- 

USER PART
---------

rulesPresenter 
	in this widget there is 'someRules' 
	+ right click open a contextMenu see TAG #menu for more information 
	
criticsPresenter 
	in this widget there is all location of broken rule
	+ right click open a contextMenu see TAG #menu for more information 

noteBookPageCriticErrorPresenter ( critic location )
	will show the error location 
	
noteBookPageRuleCommentPresenter ( comment rule )
	will show you a comment about the selected rule
	
dev part
--------

BUTTON
======
	go to TAG: #utilities-buttons and see the root class for more information about adding button

tab
=== 

	go to TAG: #utilities-tabContent and see the root class for more information about tab content

CONTEXT-MENU 
============

	go to TAG: #utilities-contextMenu for more information about contextMenu 
	and if you want to add contextMenu with Commander2  see the official documentation
	
Internal Representation and Key Implementation Points.

    Instance Variables
	criticsPresenter: <SpListPresenter>
	rulesPresenter: <SpTreeTablePresenter> 
	rbEnvironment : <RBBrowserEnvironment>
	removeTestCase : <Boolean>
		it's use to 
		
	criticActionBar : <SpActionBarPresenter> 	
	ruleActionBar : <SpActionBarPresenter>
		those 2 bar is use to show collection of button
	
	criticCache : <CriticsCache>
		it's a cache use to improve loading 
		
	noteBookPresenter : <SpNotebookPresenter>
	
	noteBookPageCriticErrorPresenter : <SpComposablePresenter>
		i'm a tab use to show where is the critic selected with SpCodePresenter
	noteBookPageRuleCommentPresenter : <SpComposablePresenter>
		i'm a tab use to show a comment about the selected rule with a SpTextPresenter
		

"
Class {
	#name : #CBCriticBrowser,
	#superclass : #SpPresenter,
	#instVars : [
		'criticsPresenter',
		'rulesPresenter',
		'rbEnvironment',
		'removeTestCase',
		'noteBookPresenter',
		'criticCache',
		'noteBookPageCriticErrorPresenter',
		'noteBookPageRuleCommentPresenter',
		'criticActionBar',
		'ruleActionBar'
	],
	#category : #'Tool-CriticBrowser-Base'
}

{ #category : #commands }
CBCriticBrowser class >> buildCommandsGroupWith: presenter forRoot: rootCommandGroup [
	rootCommandGroup
		register:
			((CmCommandGroup named: 'critic') asSpecGroup
				register: (CBBrowseCriticCommand forSpec context: presenter);
				register: (CBImplementorsCriticCommand forSpec context: presenter);
				register: (CBMarkAsTODOCriticCommand forSpec context: presenter);
				register: (CBMarkAsWrongCriticCommand forSpec context: presenter);
				register: (CBSenderCriticCommand forSpec context: presenter);
				yourself);
		register:
			((CmCommandGroup named: 'rule') asSpecGroup
				register: (CBBrowseRuleCommand forSpec context: presenter);
				register: (CBBanRuleCommand forSpec context: presenter);
				register: (CBReapplyRuleCommand forSpec context: presenter);
				register: (CBUnBanRuleCommand forSpec context: presenter);
				yourself)
]

{ #category : #'world menu' }
CBCriticBrowser class >> criticsBrowserMenuOn: aBuilder [
	<worldMenu>
	(aBuilder item: 'Critic Browser')
		action: [ self openOnCurrentWorkingConfiguration ];
		order: 310;
		parent: #Tools;
		help: 'To manage rule checks.';
		icon: self icon
]

{ #category : #specs }
CBCriticBrowser class >> defaultSpec [
	^ SpPanedLayout newVertical
		add:
			(SpPanedLayout newHorizontal
				add:
					(SpBoxLayout newVertical
						add: #rulesPresenter;
						add: #ruleActionBar
							withConstraints: [ :element | element height: self buttonHeight ];
						yourself);
				add:
					(SpBoxLayout newVertical
						add: #criticsPresenter;
						add: #criticActionBar
							withConstraints: [ :element | element height: self buttonHeight ];
						yourself);
				yourself)
			withConstraints: [ :elt | 
				elt
					beNotResizable;
					beShrinkable ];
		add: #noteBookPresenter;
		yourself
]

{ #category : #example }
CBCriticBrowser class >> example [
	<example>
	self openOnRules: RBCompositeLintRule allGoodRules rules environment: RBBrowserEnvironment default
]

{ #category : #accessing }
CBCriticBrowser class >> icon [
	"Answer an icon for the receiver."

	^ self iconNamed: #smallWarningIcon
]

{ #category : #'instance creation' }
CBCriticBrowser class >> openOnCurrentWorkingConfiguration [
	CriticWorkingConfiguration exists ifTrue: [ ResetWindow new openModalWithSpec ] ifFalse: [ SelectPackageBrowser open ]
]

{ #category : #'instance creation' }
CBCriticBrowser class >> openOnRules: aCollectionOfRules environment: anEnvironment [
	^ self openOnRules: aCollectionOfRules environment: anEnvironment removingTestCase: false
]

{ #category : #'instance creation' }
CBCriticBrowser class >> openOnRules: aRules environment: anEnvironment removingTestCase: aBoolean [
	| cbr |
	cbr := self new
		rules: aRules;
		environment: anEnvironment;
		removeTestCase: aBoolean;
		yourself.
	cbr applyRules.
	cbr openWithSpec.
	^ cbr
]

{ #category : #'instance creation' }
CBCriticBrowser class >> openOnWorkingConfiguration: aWorkingConfiguration [
	^ self openOnRules: aWorkingConfiguration rules environment: aWorkingConfiguration environment removingTestCase: aWorkingConfiguration removeTestCase
]

{ #category : #utilities }
CBCriticBrowser >> addCriticToToDo [
	criticsPresenter selectedItems
		ifNotNil:
			[ :items | items do: [ :item | criticCache addToDo: item forRule: self rule ] ]
]

{ #category : #utilities }
CBCriticBrowser >> addFalsePositive: aCritic forRule: aRule [
	criticCache addFalsePositive: aCritic forRule: aRule
]

{ #category : #utilities }
CBCriticBrowser >> addFalsePositiveRule: aRule forPackage: aPackage [
	criticCache addFalsePositiveRule: aRule forPackage: aPackage
]

{ #category : #utilities }
CBCriticBrowser >> addToDo: aCritic forRule: aRule [
	criticCache addToDo: aCritic forRule: aRule
]

{ #category : #utilities }
CBCriticBrowser >> allPackages [ 
	^ rbEnvironment packages
]

{ #category : #utilities }
CBCriticBrowser >> allRules [
	^ rulesPresenter roots flatCollect: #rules
]

{ #category : #utilities }
CBCriticBrowser >> applyRules [
	| rules checker |
	rules := self allRules.
	criticCache checker: ReSmalllintChecker new.
	checker := criticCache checker.
	rbEnvironment packages
		do: [ :package | 
			checker
				runRules: rules
				onPackage: package
				withoutTestCase: removeTestCase.
			checker rule: rules.
			criticCache packages: rbEnvironment.
			criticCache initCache.
			self registerToAnnouncements ]
]

{ #category : #accessing }
CBCriticBrowser >> cache:  aCache [ 
	criticCache := aCache
]

{ #category : #accessing }
CBCriticBrowser >> checker [
	^ criticCache checker
]

{ #category : #'system annoucements-Action' }
CBCriticBrowser >> classAdded: aClass [

	| rules |

	(rbEnvironment definesClass: aClass) ifFalse: [ ^ self ].

	rules := self allRules.
	self checker
		resetResult;
		checkClass: aClass.
		
	rules do: [ :rule |		
		(self checker criticsOf: rule) do:	[ :crit |
			criticCache  addCritic: crit forRule: rule ].
		(self checker falsePositiveOf: rule) do:	[ :crit |
			criticCache  addFalsePositive: crit forRule: rule ].
		(self checker toDoOf: rule) do:	[ :crit |
			criticCache addToDo: crit forRule: rule ] ].
	
	criticCache updateBrowser.
]

{ #category : #'system annoucements-Action' }
CBCriticBrowser >> classRemoved: aClass [
	(rbEnvironment definesClass: aClass)
		ifTrue: [ criticCache itemRemoved: aClass ].
	criticCache updateBrowser
]

{ #category : #utilities }
CBCriticBrowser >> computeToSort: aRules [
	| total falsePositives toDos |
	falsePositives := (criticCache falsePositiveOf: aRules) size.
	toDos := (criticCache toDosOf: aRules) size.
	total := (criticCache criticsOf: aRules) size.
	^ total - falsePositives - toDos
]

{ #category : #accessing }
CBCriticBrowser >> criticCache [
	^ criticCache
]

{ #category : #utilities }
CBCriticBrowser >> criticsOf: aRule [
	^ criticCache criticsOf: aRule
]

{ #category : #accessing }
CBCriticBrowser >> criticsOfSelectedRule: aRule [
	^ (aRule isNil or: [ aRule isComposite ]) ifTrue: [ #() ] ifFalse: [ (self criticsOf: aRule) sorted: #printString ascending ]
]

{ #category : #accessing }
CBCriticBrowser >> criticsPresenter [
	^ criticsPresenter
]

{ #category : #'initialize-actions' }
CBCriticBrowser >> criticsPresenterAction [
	(criticsPresenter
		transmitTo: noteBookPageCriticErrorPresenter
		transform:
			[ :selectedCritic | selectedCritic ifNotNil: [ selectedCritic providesChange ifTrue: [ self diffTextForChange: selectedCritic change ] ifFalse: [ selectedCritic entity definitionForCriticBrowser ] ] ifNil: [ '' ] ])
		postTransmission: [ :notebookPage :critics :selectedCritic | 
			noteBookPresenter selectPageIndex: 2.
			selectedCritic
				ifNotNil: [ :critic | 
					critic providesChange ifFalse: [ notebookPage behavior: (critic entity isCompiledMethod ifTrue: [ critic entity methodClass ] ifFalse: [ nil ]) ].
					critic sourceAnchor providesInterval ifTrue: [ notebookPage setSelection: critic sourceAnchor interval ] ] ];
		preTransmission: [ :notebookPage :critics :selectedCritic | notebookPage syntaxHighlight: (selectedCritic isNotNil and: [ selectedCritic providesChange not ]) ]
]

{ #category : #utilities }
CBCriticBrowser >> diffSeparator [

	^ '——————————————————
	
'
]

{ #category : #utilities }
CBCriticBrowser >> diffTextForChange: aRefactoryChange [
	| text builder |

	text := Text new.
	builder := PrettyTextDiffBuilder new.
	aRefactoryChange changes
		do: [ :chng |
			builder
				from: chng oldVersionTextToDisplay
				to: chng textToDisplay.
			text append: builder buildDisplayPatch ]
		separatedBy: [ text append: self diffSeparator ].
	
	^ text
]

{ #category : #accessing }
CBCriticBrowser >> environment: aEnvironment [
	rbEnvironment := aEnvironment
 
]

{ #category : #utilities }
CBCriticBrowser >> formatCritic: aCritic [
	
	^  String streamContents: [:s | aCritic sourceAnchor entity criticNameOn: s ]

]

{ #category : #'system Annoucements-Handle' }
CBCriticBrowser >> handleClassAdded: anAnnouncement [
	self classAdded: anAnnouncement classAdded
]

{ #category : #'system Annoucements-Handle' }
CBCriticBrowser >> handleClassModified: anAnnouncement [
	self
		classRemoved: anAnnouncement newClassDefinition;
		classAdded: anAnnouncement newClassDefinition
]

{ #category : #'system Annoucements-Handle' }
CBCriticBrowser >> handleClassRemoved: anAnnouncement [
	self classRemoved: anAnnouncement classRemoved
]

{ #category : #'system Annoucements-Handle' }
CBCriticBrowser >> handleMethodAdded: anAnnouncement [
	self methodAdded: anAnnouncement methodAdded
]

{ #category : #'system Annoucements-Handle' }
CBCriticBrowser >> handleMethodModified: anAnnouncement [
	self
		methodRemoved: anAnnouncement newMethod;
		methodAdded: anAnnouncement newMethod
]

{ #category : #'system Annoucements-Handle' }
CBCriticBrowser >> handleMethodRemoved: anAnnouncement [
	self methodRemoved: anAnnouncement methodRemoved
]

{ #category : #utilities }
CBCriticBrowser >> iconFor: anItem [
	"It is weird than in a branch of the condition there is no icon returned"
	^ (criticCache isFalsePositive: anItem forRule: self rule)
		ifFalse: [ self
				iconNamed:
					((criticCache isToDo: anItem forRule: self rule)
						ifTrue: [ #repair ]
						ifFalse: [ #error ]) ]
		ifTrue: [ self iconNamed:  #changeRemove ]
]

{ #category : #initialization }
CBCriticBrowser >> initialize [
	super initialize.
	criticCache := ReCriticsCache new.
	criticCache
		checker: ReSmalllintChecker new;
		browser: self
]

{ #category : #'initialize-widgets' }
CBCriticBrowser >> initializeCriticErrorPresenter [
	noteBookPageCriticErrorPresenter
		acceptBlock: [ :text | 
			| selectedItem |
			selectedItem := self criticsPresenter selection selectedItem
				sourceAnchor entity.
			selectedItem isCompiledMethod
				ifTrue: [ selectedItem methodClass compile: text ]
				ifFalse: [ Smalltalk compiler evaluate: text ]	"and: [ rule isTransformationRule not ]" ]
]

{ #category : #'initialize-widgets' }
CBCriticBrowser >> initializeCriticsPresenter [
	criticsPresenter
		displayBlock: [ :selec | self formatCritic: selec ];
		itemFilter: [ :item :string | '*' , string , '*' match: item contents ];
		contextMenu: [ (self rootCommandsGroup / 'critic') beRoot asMenuPresenter ];
		icons: [ :item | self iconFor: item ]
]

{ #category : #'initialize-widgets' }
CBCriticBrowser >> initializeNoteBookPresenter [
	noteBookPresenter
		addPage: (SpNotebookPage title: 'Rule comment' provider: [ noteBookPageRuleCommentPresenter ]);
		addPage: (SpNotebookPage title: 'Critic location' provider: [ noteBookPageCriticErrorPresenter ])
]

{ #category : #initialization }
CBCriticBrowser >> initializePresenter [
	self criticsPresenterAction.
	self rulesPresenterActions.
]

{ #category : #'initialize-widgets' }
CBCriticBrowser >> initializeRulesPresenter [
	rulesPresenter
		addColumn: (SpStringTableColumn title: 'Rule' evaluated: #name);
		addColumn:
			((SpStringTableColumn title: 'To sort' evaluated: [ :aRules | self computeToSort: aRules ])
				width: 60;
				yourself);
		addColumn:
			((SpStringTableColumn
				title: 'To do';
				evaluated: [ :aRules | (criticCache toDosOf: aRules) size ])
				width: 60;
				yourself);
		addColumn:
			((SpStringTableColumn title: 'False positive' evaluated: [ :aRules | (criticCache falsePositiveOf: aRules) size ])
				width: 60;
				yourself);
		yourself.
	rulesPresenter
		children: [ :aRule | aRule isComposite ifTrue: [ aRule rules ] ifFalse: [ #() ] ];
		contextMenu: [ (self rootCommandsGroup / 'rule') beRoot asMenuPresenter ]
]

{ #category : #initialization }
CBCriticBrowser >> initializeWidgets [
	criticsPresenter := self newList.
	noteBookPresenter := self newNotebook.
	criticActionBar := self newActionBar.
	ruleActionBar := self newActionBar.
	rulesPresenter := self newTreeTable.
	noteBookPageRuleCommentPresenter := self newText.
	noteBookPageCriticErrorPresenter := self newCode.

	ruleActionBar addAll: (CBAbstractRuleAction actions collect: [ :class | self newButtonFor: class ]).
	criticActionBar addAll: (CBAbstractCriticAction actions collect: [ :class | self newButtonFor: class ]).

	self initializeNoteBookPresenter.
	self initializeCriticErrorPresenter.
	self initializeRulesPresenter.
	self initializeCriticsPresenter
]

{ #category : #initialization }
CBCriticBrowser >> initializeWindow: aWindowPresenter [
	aWindowPresenter
		initialExtent: 900 @ 700;
		title: 'Critic Browser';
		whenClosedDo: [ self unregisterFromAnnouncements ]
]

{ #category : #testing }
CBCriticBrowser >> isSelectedCriticIsToDo [
	^ criticCache isToDo: self selectedCritic forRule: self selectedRule
]

{ #category : #testing }
CBCriticBrowser >> isSelectedRuleIsFalsePositive [
	^ criticCache
		isFalsePositive: self selectedCritic
		forRule: self selectedRule
]

{ #category : #utilities }
CBCriticBrowser >> logInManifest [
	criticCache logInManifest 
]

{ #category : #utilities }
CBCriticBrowser >> markAsFalsePositiveForCurrentRule: aCritic [
	criticCache addFalsePositive: aCritic forRule: self rule
]

{ #category : #'system annoucements-Action' }
CBCriticBrowser >> methodAdded: aMethod [
	| rules |
	(rbEnvironment includesMethod: aMethod)
		ifFalse: [ ^ self ].
	rules := self allRules.
	self checker
		resetResult;
		getCritiquesAbout: aMethod
			by: (rules select: [ :r | r class checksMethod ]).
	rules
		do: [ :rule | 
			(self checker criticsOf: rule)
				do: [ :crit | criticCache addCritic: crit forRule: rule ].
			(self checker falsePositiveOf: rule)
				do: [ :crit | criticCache addFalsePositive: crit forRule: rule ].
			(self checker toDoOf: rule)
				do: [ :crit | criticCache addToDo: crit forRule: rule ] ].
	criticCache updateBrowser
]

{ #category : #'system annoucements-Action' }
CBCriticBrowser >> methodRemoved: aMethod [
	(rbEnvironment includesMethod: aMethod)
		ifTrue: [ criticCache itemRemoved: aMethod ].
	criticCache updateBrowser
]

{ #category : #'initialize-widgets' }
CBCriticBrowser >> newButtonFor: aClass [ 
	^ self newButton 
		label: aClass title;
		action: [ aClass runOn: self ];
		yourself
]

{ #category : #accessing }
CBCriticBrowser >> noteBookPageCriticErrorPresenter [
	^ noteBookPageCriticErrorPresenter
]

{ #category : #accessing }
CBCriticBrowser >> noteBookPageRuleCommentPresenter [
	^ noteBookPageRuleCommentPresenter
]

{ #category : #accessing }
CBCriticBrowser >> noteBookPresenter [
	^ noteBookPresenter
]

{ #category : #'system Annoucements-Handle' }
CBCriticBrowser >> onWindowClosed [
	self unregisterFromAnnouncements.
	criticCache cacheChange
		ifTrue: [ (UIManager default
				confirm:
					'Do you want log all wrong violations in the Manifests
before closing the Critics Browser ?')
				ifTrue: [ criticCache logInManifest ] ]
]

{ #category : #utilities }
CBCriticBrowser >> reapplyAllRules [
	criticCache initialize.
	self applyRules
]

{ #category : #utilities }
CBCriticBrowser >> reapplyRule: aRule [
	criticCache removeRule: aRule.
	criticCache checker resetResult.
	rbEnvironment packages
		do: [ :package | 
			criticCache checker
				runRules: {aRule}
				onPackage: package
				withoutTestCase: removeTestCase ].
	(criticCache checker criticsOf: aRule)
		do: [ :crit | criticCache addCritic: crit forRule: aRule ].
	(criticCache checker falsePositiveOf: aRule)
		do: [ :crit | criticCache addFalsePositive: crit forRule: aRule ].
	(criticCache checker toDoOf: aRule)
		do: [ :crit | criticCache addToDo: crit forRule: aRule ].
	criticCache updateBrowser
]

{ #category : #'system annoucements' }
CBCriticBrowser >> registerToAnnouncements [
	 
	self unregisterFromAnnouncements.
	SystemAnnouncer uniqueInstance weak
		when: ClassAdded send: #handleClassAdded: to: self;
		when: ClassModifiedClassDefinition send: #handleClassModified: to: self;
		when: ClassRemoved send: #handleClassRemoved: to: self;
		when: MethodAdded send: #handleMethodAdded: to: self;
		when: MethodModified send: #handleMethodModified: to: self;
		when: MethodRemoved send: #handleMethodRemoved: to: self.
	self announcer when: WindowClosed send: #onWindowClosed to: self" Unmatched " "in comment".
]

{ #category : #utilities }
CBCriticBrowser >> removeFalsePositive: aCritic forRule: aRule [
	criticCache removeFalsePositive: self selectedCritic forRule: self selectedRule. 
]

{ #category : #utilities }
CBCriticBrowser >> removeFalsePositiveRule: aRule forPackage: aPackage [
	criticCache removeFalsePositiveRule: aRule forPackage: aPackage
]

{ #category : #accessing }
CBCriticBrowser >> removeTestCase: aBoolean [
	removeTestCase := aBoolean 
]

{ #category : #utilities }
CBCriticBrowser >> removeToDo: aCritic forRule: aRule [
	criticCache removeToDo: aCritic forRule: aRule
]

{ #category : #utilities }
CBCriticBrowser >> rule [
	^ self rulesPresenter selection selectedItem
]

{ #category : #accessing }
CBCriticBrowser >> rules: rulesCollection [
	self rulesPresenter roots: ((
		(rulesCollection groupedBy: #group) associations collect: [ :as |
			CriticBrowserRulesGroup
				named: as key
				rules: as value ])
					sorted: [ :a :b | a name < b name ])
]

{ #category : #accessing }
CBCriticBrowser >> rulesPresenter [
	^ rulesPresenter 
]

{ #category : #'initialize-actions' }
CBCriticBrowser >> rulesPresenterActions [
	rulesPresenter
		transmitTo: criticsPresenter
		transform: [ :aRule | self criticsOfSelectedRule: aRule ]
		postTransmission: [ :destination | destination selectIndex: 1 ].
	
	rulesPresenter
		transmitTo: noteBookPageRuleCommentPresenter
		transform: [ :aRule | (aRule isNil or: [ aRule isComposite ]) ifTrue: [ '' ] ifFalse: [ aRule rationale ] ]
]

{ #category : #accessing }
CBCriticBrowser >> selectedCritic [
	^ criticsPresenter selection selectedItem
]

{ #category : #utilities }
CBCriticBrowser >> selectedRule [
	^ rulesPresenter selectedItem
]

{ #category : #utilities }
CBCriticBrowser >> selectionIntervalFor: aString [
	^ self rule result selectionIntervalFor: aString
]

{ #category : #'system annoucements' }
CBCriticBrowser >> unregisterFromAnnouncements [

	SystemAnnouncer uniqueInstance unsubscribe: self
]

{ #category : #utilities }
CBCriticBrowser >> updateTree [
"	self needRebuild: false.
	self buildWithSpec"
]

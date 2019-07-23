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
	criticsActionsPresenter: <SpActionBarPresenter> 
	rulesActionsPresenter : <SpActionBarPresenter>
	noteBookPresenter : <SpNotebookPresenter>
	criticCache : <CriticsCache>
	noteBookPageCriticErrorPresenter : <SpComposablePresenter>
	noteBookPageRuleCommentPresenter : <SpComposablePresenter>

"
Class {
	#name : #CBCriticBrowser,
	#superclass : #SpPresenter,
	#instVars : [
		'criticsPresenter',
		'rulesPresenter',
		'rbEnvironment',
		'removeTestCase',
		'criticsActionsPresenter',
		'rulesActionsPresenter',
		'noteBookPresenter',
		'criticCache',
		'noteBookPageCriticErrorPresenter',
		'noteBookPageRuleCommentPresenter'
	],
	#category : #'Tool-CriticBrowser-Base'
}

{ #category : #commands }
CBCriticBrowser class >> buildCommandsGroupWith: presenter forRoot: rootCommandGroup [
	rootCommandGroup
		register:
			((CmCommandGroup named: 'critic') asSpecGroup
				register: (CBBrowseCriticMenu forSpec context: presenter);
				register: (CBImplementorCriticMenu forSpec context: presenter);
				register: (CBMarkAsTODOCriticMenu forSpec context: presenter);
				register: (CBMarkAsWrongCriticMenu forSpec context: presenter);
				register: (CBSenderCriticMenu forSpec context: presenter);
				yourself);
		register:
			((CmCommandGroup named: 'rule') asSpecGroup
				register: (CBBanRuleMenu forSpec context: presenter);
				register: (CBBrowseRuleMenu forSpec context: presenter);
				register: (CBReapplyRuleMenu forSpec context: presenter);
				register: (CBUnBanRuleMenu forSpec context: presenter);
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
						add: #rulesActionsPresenter
							withConstraints: [ :element | element height: self buttonHeight ];
						yourself);
				add:
					(SpBoxLayout newVertical
						add: #criticsPresenter;
						add: #criticsActionsPresenter
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

{ #category : #utils }
CBCriticBrowser >> addCriticToToDo [
	criticsPresenter selectedItems
		ifNotNil:
			[ :items | items do: [ :item | criticCache addToDo: item forRule: self rule ] ]
]

{ #category : #utils }
CBCriticBrowser >> addFalsePositive: aCritic forRule: aRule [
	criticCache addFalsePositive: aCritic forRule: aRule
]

{ #category : #utils }
CBCriticBrowser >> addFalsePositiveRule: aRule forPackage: aPackage [
	criticCache addFalsePositiveRule: aRule forPackage: aPackage
]

{ #category : #utils }
CBCriticBrowser >> addToDo: aCritic forRule: aRule [
	criticCache addToDo: aCritic forRule: aRule
]

{ #category : #utils }
CBCriticBrowser >> allPackages [ 
	^ rbEnvironment packages
]

{ #category : #utils }
CBCriticBrowser >> allRules [
	^ rulesPresenter roots flatCollect: #rules
]

{ #category : #utils }
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

{ #category : #utils }
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

{ #category : #utils }
CBCriticBrowser >> criticsOf: aRule [
	^ criticCache criticsOf: aRule
]

{ #category : #accessing }
CBCriticBrowser >> criticsPresenter [
	^ criticsPresenter
]

{ #category : #'initialize-actions' }
CBCriticBrowser >> criticsPresenterAction [
	criticsPresenter
		whenActivatedDo:
			[ :singleSelection | self doubleClickActionOnCritic: singleSelection ].
	criticsPresenter
		whenSelectionChangedDo: [ :singleSelection | 
			self doubleClickActionOnCritic: singleSelection.
			self send: criticsPresenter to: noteBookPageCriticErrorPresenter ]
]

{ #category : #utils }
CBCriticBrowser >> defaultColor [
	^ Color red
]

{ #category : #utils }
CBCriticBrowser >> diffSeparator [

	^ '——————————————————
	
'
]

{ #category : #utils }
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

{ #category : #'initialize-actions' }
CBCriticBrowser >> doubleClickActionOnCritic: singleSelection [
	| selectedCritic |
	selectedCritic := singleSelection selectedItem.
	selectedCritic
		ifNotNil: [ selectedCritic providesChange
				ifTrue: [ noteBookPageCriticErrorPresenter 
						text: (self diffTextForChange: selectedCritic change) ]
				ifFalse: [ | entity |
					entity := selectedCritic sourceAnchor entity.
					(entity respondsTo: #definition)
						ifTrue: [ noteBookPageCriticErrorPresenter text: entity definition ]
						ifFalse: [ noteBookPageCriticErrorPresenter text: entity asString ] ].
			noteBookPresenter selectPageIndex: 2 ]
		ifNil: [ '' ]
]

{ #category : #accessing }
CBCriticBrowser >> environment: aEnvironment [
	rbEnvironment := aEnvironment
 
]

{ #category : #utils }
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

{ #category : #utils }
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

{ #category : #initialize }
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
		addPage:
			(NotebookPage title: 'Rule comment' provider: [ noteBookPageRuleCommentPresenter ]).
	noteBookPresenter
		addPage:
			(NotebookPage
				title: 'Critic location'
				provider: [ noteBookPageCriticErrorPresenter ])
]

{ #category : #initialize }
CBCriticBrowser >> initializePresenter [
	self criticsPresenterAction.
	self rulesPresenterActions.
]

{ #category : #'initialize-widgets' }
CBCriticBrowser >> initializeRulesPresenter [
	rulesPresenter
		addColumn:
			(StringTableColumn new
				title: 'Rule';
				evaluated: #name;
				yourself);
		addColumn:
			(StringTableColumn new
				width: 60;
				title: 'To sort';
				evaluated: [ :aRules | self computeToSort: aRules ];
				yourself);
		addColumn:
			(StringTableColumn new
				width: 60;
				title: 'To do';
				evaluated: [ :aRules | (criticCache toDosOf: aRules) size ];
				yourself);
		addColumn:
			(StringTableColumn new
				width: 60;
				title: 'False positive';
				evaluated: [ :aRules | (criticCache falsePositiveOf: aRules) size ];
				yourself);
		yourself.
	rulesPresenter
		children: [ :aRule | 
			aRule isComposite
				ifTrue: [ aRule rules ]
				ifFalse: [ #() ] ];
		contextMenu: [ (self rootCommandsGroup / 'rule') beRoot asMenuPresenter ]
]

{ #category : #initialize }
CBCriticBrowser >> initializeWidgets [
	criticsPresenter := self newList.
	noteBookPresenter := self newNotebook.
	criticsActionsPresenter := self newActionBar.
	rulesActionsPresenter := self newActionBar.
	rulesPresenter := self newTreeTable.
	noteBookPageRuleCommentPresenter := self newText.
	noteBookPageCriticErrorPresenter := self newCode.
	CBAbstractRuleAction actions
		do: [ :class | rulesActionsPresenter add: (self newButtonFor: class) ].
	CBAbstractCriticAction actions
		do: [ :class | criticsActionsPresenter add: (self newButtonFor: class) ].
	self initializeNoteBookPresenter.
	self initializeCriticErrorPresenter.	"and: [ rule isTransformationRule not ]"
	self initializeRulesPresenter.
	self initializeCriticsPresenter
]

{ #category : #initialize }
CBCriticBrowser >> initializeWindow: aWindowPresenter [
	aWindowPresenter
		initialExtent: 900 @ 700;
		title: 'Critic Browser'
]

{ #category : #tests }
CBCriticBrowser >> isSelectedCriticIsToDo [
	^ criticCache isToDo: self selectedCritic forRule: self selectedRule
]

{ #category : #tests }
CBCriticBrowser >> isSelectedRuleIsFalsePositive [
	^ criticCache
		isFalsePositive: self selectedCritic
		forRule: self selectedRule
]

{ #category : #utils }
CBCriticBrowser >> logInManifest [
	criticCache logInManifest 
]

{ #category : #utils }
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

{ #category : #utils }
CBCriticBrowser >> reapplyAllRules [
	criticCache initialize.
	self applyRules
]

{ #category : #utils }
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

{ #category : #utils }
CBCriticBrowser >> removeFalsePositive: aCritic forRule: aRule [
	criticCache removeFalsePositive: self selectedCritic forRule: self selectedRule. 
]

{ #category : #utils }
CBCriticBrowser >> removeFalsePositiveRule: aRule forPackage: aPackage [
	criticCache removeFalsePositiveRule: aRule forPackage: aPackage
]

{ #category : #accessing }
CBCriticBrowser >> removeTestCase: aBoolean [
	removeTestCase := aBoolean 
]

{ #category : #utils }
CBCriticBrowser >> removeToDo: aCritic forRule: aRule [
	criticCache removeToDo: aCritic forRule: aRule
]

{ #category : #utils }
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
		transform: [ :aRule | 
			(aRule isNil or: [ aRule isComposite ])
				ifTrue: [ #() ]
				ifFalse: [ (criticCache criticsOf: aRule) sorted: #printString ascending ] ].
	rulesPresenter
		whenSelectionChangedDo: [ :singleSelection | 
			| selectedRule |
			selectedRule := singleSelection selectedItem.
			(selectedRule isNil or: [ selectedRule isComposite ])
				ifTrue: [ noteBookPageRuleCommentPresenter text: '' ]
				ifFalse: [ noteBookPageRuleCommentPresenter text: selectedRule rationale ].
			noteBookPresenter selectPageIndex: 1 ]
]

{ #category : #accessing }
CBCriticBrowser >> selectedCritic [
	^ criticsPresenter selection selectedItem
]

{ #category : #utils }
CBCriticBrowser >> selectedRule [
	^ rulesPresenter selectedItem
]

{ #category : #utils }
CBCriticBrowser >> selectionIntervalFor: aString [
	^ self rule result selectionIntervalFor: aString
]

{ #category : #utils }
CBCriticBrowser >> send: origin to: dest [
	^ origin selection selectedItem
		ifNotNil: [ :critic | 
			critic providesChange
				ifTrue: [ dest behavior: nil ]
				ifFalse: [ | entity |
					entity := critic sourceAnchor entity.
					self
						flag:
							'Another hack. We need a better way to display entities (because now we may get not only classes and methods. Maybe something like GTInspector presentations could work)'.
					dest
						behavior:
							(entity isCompiledMethod
								ifTrue: [ entity methodClass ]
								ifFalse: [ nil ]).
					critic sourceAnchor providesInterval
						ifTrue: [ dest setSelection: critic sourceAnchor interval ] ] ]
]

{ #category : #'system annoucements' }
CBCriticBrowser >> unregisterFromAnnouncements [

	SystemAnnouncer uniqueInstance unsubscribe: self
]

{ #category : #utils }
CBCriticBrowser >> updateTree [
	self needRebuild: false.
	self buildWithSpec
]

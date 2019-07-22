"
this class is a merge of CriticBrowser and SpecSingleCodeCriticResult (used by old CriticBrowser )
so 
			new Layout 
        ---------------------------------------------
        | RulesPresenter  ||                        |
        |                 ||                        |
        |                 || CriticsPresenter       |
        |-----------------||------------------------|
        |reset |update|log||browse|Tranform|marknoPB|                       |              |
        |===========================================|
        |SourceCodePresenter                        |
        ---------------------------------------------
	      now i use the filter feature of a ListPresenter (CriticsPresenter) instead of TextInputFieldPresenter
			

For the Class part:  State a one line summary. For example, ""I represent a paragraph of text"".

For the Responsibility part: Three sentences about my main responsibilities - what I do, what I know.

For the Collaborators Part: State my main collaborators and one line about how I interact with them. 

Public API and Key Messages

- message one   
- message two 
- (for bonus points) how to create instances.

   One simple example is simply gorgeous.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	cache:		<Object>
	checker:		<Object>
	criticsPresenter:		<Object>
	logButton:		<Object>
	rbEnvironment:		<Object>
	removeTestCase:		<Object>
	resetButton:		<Object>
	rule:		<Object>
	rulesPresenter:		<Object>
	sourceCodePresenter:		<Object>
	thread:		<Object>
	toolbarPresenter:		<Object>
	updateButton:		<Object>


    Implementation Points
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

{ #category : #commander2 }
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
CBCriticBrowser class >> open [
	<example>
	| env rules |
	rules := RBCompositeLintRule allGoodRules rules.
	env := RBBrowserEnvironment default.
	self openOnRule: rules onEnvironment: env
]

{ #category : #'instance creation' }
CBCriticBrowser class >> open: aWindowPresenter withExtent: aPoint [ 
	aWindowPresenter extent: aPoint.
]

{ #category : #'instance creation' }
CBCriticBrowser class >> openOnRule: aRules onEnvironment: aEnv [
	| cbr |
	cbr := self new
		rules: aRules;
		environment: aEnv;
		removeTestCase: false;
		yourself.
	cbr applyRules.
	self open: cbr openWithSpec withExtent: 700 @ 700.
	^ cbr
]

{ #category : #'instance creation' }
CBCriticBrowser class >> openOnWorkingConfiguration: aWorkingConfiguration [
	| cbr |
	cbr := self new
		rules: aWorkingConfiguration rule;
		environment: aWorkingConfiguration environment;
		removeTestCase: aWorkingConfiguration removeTestCase;
		yourself.
	cbr openWithSpec.
	cbr applyRules.
	cbr rulesModel changed: #listElementAt:
]

{ #category : #specs }
CBCriticBrowser class >> title [

	^ 'Critic Browser'
]

{ #category : #'initialize-actions' }
CBCriticBrowser >> DoubleClickActionOnCritic: singleSelection [
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
			[ :singleSelection | self DoubleClickActionOnCritic: singleSelection ].
	criticsPresenter
		whenSelectionChangedDo: [ :singleSelection | 
			self DoubleClickActionOnCritic: singleSelection.
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
	criticCache checker: ReSmalllintChecker new.
	criticCache browser: self
]

{ #category : #'initialize-widgets' }
CBCriticBrowser >> initializeCriticErrorPresenter [
	noteBookPageCriticErrorPresenter
		acceptBlock: [ :text | 
			| selectedItem |
			selectedItem := self criticsPresenter selectedItem sourceAnchor
				entity.
			selectedItem isCompiledMethod "and: [ rule isTransformationRule not ]"
				ifTrue: [ selectedItem methodClass compile: text ]
				ifFalse: [ Smalltalk compiler evaluate: text ]	 ]
]

{ #category : #'initialize-widgets' }
CBCriticBrowser >> initializeCriticsPresenter [
	criticsPresenter
		displayBlock: [ :selec | self formatCritic: selec ];
		itemFilterBlock: [ :item :string | '*' , string , '*' match: item contents ];
		contextMenu: [(self rootCommandsGroup / 'critic') beRoot
				asMenuPresenter];
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
				title: 'rules';
				evaluated: #name;
				yourself);
		addColumn:
			(StringTableColumn new
				width: 40;
				title: 'toSort';
				evaluated: [ :aRules | self computeToSort: aRules ];
				yourself);
		addColumn:
			(StringTableColumn new
				width: 40;
				title: 'toDo';
				evaluated: [ :aRules | (criticCache toDosOf: aRules) size ];
				yourself);
		addColumn:
			(StringTableColumn new
				width: 40;
				title: 'Wrong';
				evaluated: [ :aRules | (criticCache falsePositiveOf: aRules) size ];
				yourself);
		yourself.
	rulesPresenter
		children: [ :aRule | 
			aRule isComposite
				ifTrue: [ aRule rules ]
				ifFalse: [ #() ] ];
		contextMenu: [ (self rootCommandsGroup / 'critic') asMenuPresenter ]
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

"
I am a presenter that allows get a list of critiques that broken the selected rules in the selected packages. 

I interact with CBSelectPackage to get the list of selected packages.
I interact with CBSelectRule to get the list of selected rules.
I interact with CBritiquesCache to get the critiques saved as 'To-do' and marked as 'false positive'.

You can open me whit:
`CBCritiqueBrowser open`

"
Class {
	#name : #CBCritiqueBrowser,
	#superclass : #SpPresenter,
	#instVars : [
		'criticModel',
		'rulesModel',
		'checker',
		'title',
		'rbEnvironment',
		'removeTestCase',
		'cache',
		'resetButton',
		'logButton',
		'updateButton',
		'criticsModel',
		'sourceCodeModel',
		'toolbarModel'
	],
	#category : #'Tool-CriticBrowser-Base'
}

{ #category : #menu }
CBCritiqueBrowser class >> criticsBrowserMenuOn: aBuilder [ 
	<worldMenu>
	
	(aBuilder item: 'Critique Browser')
		action: [ self openOnCurrentWorkingConfiguration];
		order: 2;
		parent: #Browsing;
		help: 'To manage rule checks.';
		icon: self icon
]

{ #category : #specs }
CBCritiqueBrowser class >> defaultSpec [

	^ SpPanedLayout newTopToBottom
		  add: (SpPanedLayout newLeftToRight
				   add: (SpBoxLayout newTopToBottom
						    add: #rulesModel;
						    add: (SpBoxLayout newLeftToRight
								     add: #resetButton;
								     add: #updateButton;
								     add: #logButton;
								     yourself)
						    height: 25;
						    yourself);
				   add: (SpBoxLayout newTopToBottom
						    add: #criticsModel;
						    add: #toolbarModel
						    height: 25;
						    yourself);
				   yourself);
		  add: #sourceCodeModel;
		  yourself
]

{ #category : #icons }
CBCritiqueBrowser class >> icon [

	"Answer an icon for the receiver."

	^ self iconNamed: #smallWarningIcon
]

{ #category : #'instance creation' }
CBCritiqueBrowser class >> open [
	<example>
	^ self new openWithSpec
]

{ #category : #menu }
CBCritiqueBrowser class >> openOnCurrentWorkingConfiguration [

	<script>
	CBCritiqueWorkingConfiguration exists
		ifTrue: [ CBResetWindow new openWithSpec ]
		ifFalse: [ CBSelectPackage open ]
]

{ #category : #'instance creation' }
CBCritiqueBrowser class >> openOnRule: aRule onEnvironment: aEnv [

	| cbr |
	cbr :=  self new rules: aRule;
		environment: aEnv;
		removeTestCase: false;
		yourself.
		
	cbr openWithSpec.
	cbr applyRules. 
	cbr rulesModel changed:  #listElementAt:
]

{ #category : #'instance creation' }
CBCritiqueBrowser class >> openOnWorkingConfiguration: aWorkingConfiguration [

	| cbr |
	cbr := self new 
		rules: aWorkingConfiguration rule;
		environment: aWorkingConfiguration environment;
		removeTestCase: aWorkingConfiguration removeTestCase;
		yourself.

	cbr openWithSpec.
	cbr applyRules. 
	cbr rulesModel  changed:  #listElementAt:
]

{ #category : #icons }
CBCritiqueBrowser class >> taskbarIconName [

	^#smallWarningIcon
]

{ #category : #window }
CBCritiqueBrowser >> addModelItemsToWindowMenu: aMenu [
	"Add model-related items to the window menu"

	aMenu
		addLine;
		add: 'Clean all manifest' target: checker selector: #cleanAllManifest;
		add: 'Reapply all  rules' target: self selector: #reapplyAllRules
]

{ #category : #private }
CBCritiqueBrowser >> allRules [
	^ rulesModel roots flatCollect: #rules
]

{ #category : #private }
CBCritiqueBrowser >> applyRules [
	| packageCount nbPackage process rules |
	rules := self allRules.
	nbPackage := rbEnvironment packages size.
	packageCount := 0.
	self updateTree.
	process := [ rbEnvironment packages
		do: [ :package | 
			| windowTitle |
			packageCount := packageCount + 1.
			windowTitle := String
				streamContents: [ :s | 
					s << 'run rules on ' << package packageName << ' ('
						<< packageCount asString << '/' << nbPackage asString << ')' ].
			self setTitle: windowTitle.
			checker
				runRules: rules
				onPackage: package
				withoutTestCase: removeTestCase ].
	checker rule: rules.
	self setTitle: self defaultTitle.
	cache packages: rbEnvironment.
	cache initCache.
	self rules: (self allRules select: [ :r | self hasBrokenRules: r]).
    self rulesModel refresh.
    self rebuildLayout.
	self updateTree. ] newProcess.
	process name: 'SmallLint'.
	process resume
]

{ #category : #private }
CBCritiqueBrowser >> browseRule [

	rulesModel selectedItem ifNotNil: [ :item | ^ item browse ]
]

{ #category : #private }
CBCritiqueBrowser >> buildRuleMenu [

	^ SpMenuPresenter new
		  addGroup: [ :group | 
			  group addItem: [ :item | 
					  item
						  name: 'Browse rule' translated;
						  action: [ self browseRule ] ].
			  group addItem: [ :item | 
				  item
					  name: 'Reapply this rule' translated;
					  action: [ self reapplyThisRule ] ] ];
		  yourself
]

{ #category : #'system annoucements' }
CBCritiqueBrowser >> classAdded: aClass [

	| rules |

	(rbEnvironment definesClass: aClass) ifFalse: [ ^ self ].

	rules := self allRules.
	checker
		resetResult;
		checkClass: aClass.
		
	rules do: [ :rule |		
		(checker criticsOf: rule) do:	[ :crit |
			cache addCritic: crit forRule: rule ].
		(checker falsePositiveOf: rule) do:	[ :crit |
			cache addFalsePositive: crit forRule: rule ].
		(checker toDoOf: rule) do:	[ :crit |
			cache addToDo: crit forRule: rule ] ].
	
	cache updateBrowser.
]

{ #category : #'system annoucements' }
CBCritiqueBrowser >> classRemoved: aClass [

	(rbEnvironment definesClass: aClass)
		ifTrue: [ cache itemRemoved: aClass ].
		
	cache updateBrowser
]

{ #category : #private }
CBCritiqueBrowser >> colorForRule: aRule [

	| total |
	^ (total := cache criticsOf: aRule)
		  ifEmpty: [ criticModel falsePositiveColor ]
		  ifNotEmpty: [ 
			  (cache falsePositiveOf: aRule) = total
				  ifTrue: [ criticModel falsePositiveColor ]
				  ifFalse: [ self class theme textColor ] ]
]

{ #category : #initialization }
CBCritiqueBrowser >> connectPresenters [

	rulesModel whenSelectedItemChangedDo: [ :rule | 
		(rule isNil or: [ rule isComposite ]) ifFalse: [ 
			criticModel
				resetSelection;
				rule: rule;
				setTextModelForNil ].
		self setTitle:
			(rule ifNil: [ self defaultTitle ] ifNotNil: [ rule name ]) ]
]

{ #category : #accessing }
CBCritiqueBrowser >> criticsModel [

	^ criticModel criticsModel
]

{ #category : #api }
CBCritiqueBrowser >> defaultTitle [

	^ 'Critique Browser'
]

{ #category : #accessing }
CBCritiqueBrowser >> environment [
	^ rbEnvironment

]

{ #category : #accessing }
CBCritiqueBrowser >> environment: aEnv [
	rbEnvironment := aEnv

]

{ #category : #'system annoucements' }
CBCritiqueBrowser >> handleClassAdded: anAnnouncement [

	self classAdded: anAnnouncement classAdded

]

{ #category : #'system annoucements' }
CBCritiqueBrowser >> handleClassModified: anAnnouncement [

	self
		classRemoved: anAnnouncement oldClassDefinition;
		classAdded: anAnnouncement newClassDefinition
	
]

{ #category : #'system annoucements' }
CBCritiqueBrowser >> handleClassRemoved: anAnnouncement [

	self classRemoved: anAnnouncement classRemoved

]

{ #category : #'system annoucements' }
CBCritiqueBrowser >> handleMethodAdded: anAnnouncement [

	self methodAdded: anAnnouncement methodAdded

]

{ #category : #'system annoucements' }
CBCritiqueBrowser >> handleMethodModified: anAnnouncement [

	self
		methodRemoved: anAnnouncement oldMethod;
		methodAdded: anAnnouncement newMethod
	

]

{ #category : #'system annoucements' }
CBCritiqueBrowser >> handleMethodRemoved: anAnnouncement [

	self methodRemoved: anAnnouncement methodRemoved

]

{ #category : #'system annoucements' }
CBCritiqueBrowser >> hasBrokenRules: rule [
    | unclassified falsePositives toDos total |
    falsePositives := (cache falsePositiveOf: rule) size.
    toDos := (cache toDosOf: rule) size.
    total := (cache criticsOf: rule) size.
    unclassified := total - falsePositives - toDos.
    ^ unclassified > 0
]

{ #category : #initialization }
CBCritiqueBrowser >> initialize [

	super initialize.
	cache := CBCritiquesCache new.
	checker := ReSmalllintChecker new.
	cache checker: checker.
	cache browser: self.
	criticModel cache: cache.
	toolbarModel cache: cache
]

{ #category : #initialization }
CBCritiqueBrowser >> initializePresenters [

	title := self defaultTitle.
	rulesModel := self newTreeTable.
	resetButton := self newButton.
	updateButton := self newButton.
	logButton := self newButton.

	self setLogButton.
	self setResetButton.
	self setUpdateButton.

	criticModel := CBSingleCodeCritiqueResultList new.
	criticsModel := self criticsModel.
	sourceCodeModel := self sourceCodeModel.
	toolbarModel := self toolbarModel.
	
	self setRulesModel.

	self focusOrder
		add: rulesModel;
		add: criticModel
]

{ #category : #'user interface' }
CBCritiqueBrowser >> initializeWindow: aWindowPresenter [

	aWindowPresenter
		title: self defaultTitle;
		initialExtent: 760 @ 370
]

{ #category : #accessing }
CBCritiqueBrowser >> logButton [
	^ logButton 
]

{ #category : #private }
CBCritiqueBrowser >> logInManifest [
	cache logInManifest
]

{ #category : #'system annoucements' }
CBCritiqueBrowser >> methodAdded: aMethod [

	| rules |

	(rbEnvironment includesMethod: aMethod) ifFalse: [ ^ self ].

	rules := self allRules.
	checker
		resetResult;
		getCritiquesAbout: aMethod by: (rules select: [ :r | r class checksMethod ]).

	rules do: [ :rule |		
		(checker criticsOf: rule) do:	[ :crit |
			cache addCritic: crit forRule: rule ].
		(checker falsePositiveOf: rule) do:	[ :crit |
			cache addFalsePositive: crit forRule: rule ].
		(checker toDoOf: rule) do:	[ :crit |
			cache addToDo: crit forRule: rule ] ].
	
	cache updateBrowser.
]

{ #category : #'system annoucements' }
CBCritiqueBrowser >> methodRemoved: aMethod [

	(rbEnvironment includesMethod: aMethod)
		ifTrue: [ cache itemRemoved: aMethod ].
		
	cache updateBrowser
]

{ #category : #api }
CBCritiqueBrowser >> onWindowClosed [

	self unregisterFromAnnouncements.

	cache cacheChange 
		ifTrue: [	
			(UIManager default confirm: 'Do you want log all wrong violations in the Manifests
before closing the Critics Browser ?')
				ifTrue: [ cache logInManifest ]].
	

]

{ #category : #display }
CBCritiqueBrowser >> open [

	^ self openWithSpec
]

{ #category : #private }
CBCritiqueBrowser >> reapplyAllRules [

	cache initialize.
	checker resetResult.
	self applyRules
]

{ #category : #private }
CBCritiqueBrowser >> reapplyRule: aRule [

	cache removeRule: aRule.
	checker resetResult.
	rbEnvironment packages do: [ :package | 
		checker runRules: { aRule } onPackage: package withoutTestCase: removeTestCase ]. 
	(checker criticsOf: aRule) do:	[ :crit |
		cache addCritic: crit forRule: aRule ].
	(checker falsePositiveOf: aRule) do:	[ :crit |
		cache addFalsePositive: crit forRule: aRule ].
	(checker toDoOf: aRule) do:	[ :crit |
		cache addToDo: crit forRule: aRule ].	
	
	cache updateBrowser
]

{ #category : #private }
CBCritiqueBrowser >> reapplyThisRule [

	| rule |
	rulesModel selectedItem ifNil: [ ^ self ].
	rule := rulesModel selectedItem.
	rule isComposite 
		ifTrue: [ rule rules do: [ :each | self reapplyRule: each ] ]
		ifFalse: [ self reapplyRule: rule ]
	

]

{ #category : #'system annoucements' }
CBCritiqueBrowser >> registerToAnnouncements [
	
	self unregisterFromAnnouncements.
	
	SystemAnnouncer uniqueInstance weak
		when: ClassAdded send: #handleClassAdded: to: self;
		when: ClassModifiedClassDefinition send: #handleClassModified: to: self;
		when: ClassRemoved send: #handleClassRemoved: to: self;
		when: MethodAdded send: #handleMethodAdded: to: self;
		when: MethodModified send: #handleMethodModified: to: self;
		when: MethodRemoved send: #handleMethodRemoved: to: self.
	self window window announcer when: WindowClosed send: #onWindowClosed to: self
]

{ #category : #accessing }
CBCritiqueBrowser >> removeTestCase: aBoolean [
	removeTestCase :=  aBoolean
]

{ #category : #accessing }
CBCritiqueBrowser >> resetButton [
	^ resetButton 
]

{ #category : #accessing }
CBCritiqueBrowser >> rules: rulesCollection [

	self rulesModel roots:
		(((rulesCollection groupedBy: #group) associations collect: [ :as | 
			  CBCritiqueBrowserRulesGroup named: as key rules: as value ]) sorted: [ 
			 :a 
			 :b | a name < b name ])
]

{ #category : #accessing }
CBCritiqueBrowser >> rulesModel [

	^ rulesModel
]

{ #category : #initialization }
CBCritiqueBrowser >> setActionLogButton [
	^ [ (UIManager default
		confirm:
			'Do you want to save all false positive and toDo in the Manifests ?
(this action may generate new manifest classes and make dirty your package)')
		ifTrue: [ cache logInManifest ] ]
]

{ #category : #initialization }
CBCritiqueBrowser >> setActionResetButton [

	^ [ 
	  (UIManager default confirm:
		   'Do you want to delete the current configuration
 and create a new configuration ?') ifTrue: [ 
		  self delete.
		  CBSelectPackage open ] ]
]

{ #category : #initialization }
CBCritiqueBrowser >> setActionUpdateButton [
	^ [ self reapplyAllRules ]
]

{ #category : #initialization }
CBCritiqueBrowser >> setLogButton [

	logButton 
		state: false;
		icon: (self iconNamed: #smallSave);
		label: 'Save Critics';
		action: self setActionLogButton 
]

{ #category : #initialization }
CBCritiqueBrowser >> setResetButton [

	resetButton 
		state: false;
		icon: (self iconNamed: #smallUpdate);
		label: 'Run new configuration';
		action: self setActionResetButton
]

{ #category : #'system annoucements' }
CBCritiqueBrowser >> setRulesModel [ 
rulesModel children: [ :rule | 
		rule isComposite
			ifTrue: [ rule rules asArray ]
			ifFalse: [ #(  ) ] ].

	rulesModel addColumn: ((SpStringTableColumn evaluated: [ :rule | 
			  self stringMorphForRule: rule ]) title: 'Rules Group').
	rulesModel contextMenu: self buildRuleMenu.
]

{ #category : #api }
CBCritiqueBrowser >> setTitle: aTitle [

	title := aTitle.
	self withWindowDo: [ :w | w title: aTitle ]
]

{ #category : #initialization }
CBCritiqueBrowser >> setUpdateButton [

	updateButton 
		state: false;
		icon: (self iconNamed: #glamorousRestart);
		label: 'Update';
		action: self setActionUpdateButton.
]

{ #category : #accessing }
CBCritiqueBrowser >> sourceCodeModel [

	^ criticModel sourceCodeModel
]

{ #category : #private }
CBCritiqueBrowser >> stringMorphForRule: rule [
	
	"set the amount of critiques per rule"
	| unclassified falsePositives toDos text total |
	falsePositives := (cache falsePositiveOf: rule) size.
	toDos := (cache toDosOf: rule) size.
	total := (cache criticsOf: rule) size.
	unclassified := total - falsePositives - toDos.
	text := String streamContents: [ :s | 
		        s
			        << rule name;
			        << ' (To sort: ';
			        print: unclassified;
			        << ', ToDo: ';
			        print: toDos;
			        << ', Wrong: ';
			        print: falsePositives;
			        << ')' ].
	^ text asMorph
		  color: (self colorForRule: rule);
		  yourself
]

{ #category : #accessing }
CBCritiqueBrowser >> toolbarModel [

	^ criticModel toolbarModel
]

{ #category : #'system annoucements' }
CBCritiqueBrowser >> unregisterFromAnnouncements [

	SystemAnnouncer uniqueInstance unsubscribe: self
]

{ #category : #accessing }
CBCritiqueBrowser >> updateButton [
	^ updateButton 
]

{ #category : #thread }
CBCritiqueBrowser >> updateTree [
	criticModel updateList. 
	rulesModel update
]

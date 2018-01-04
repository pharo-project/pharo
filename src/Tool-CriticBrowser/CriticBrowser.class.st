"
I display code critics, the results obtained when running a lint rule.

Example: 


| rule env |
rule :=  RBExcessiveArgumentsRule new.
env := (RBPackageEnvironment packageName: 'Manifest-Core').

(CriticBrowser openOnRule: rule onEnvironment: env).
"
Class {
	#name : #CriticBrowser,
	#superclass : #ComposablePresenter,
	#instVars : [
		'criticModel',
		'rulesModel',
		'checker',
		'title',
		'rbEnvironment',
		'removeTestCase',
		'cache',
		'resetButton',
		'logButton'
	],
	#category : #'Tool-CriticBrowser'
}

{ #category : #menu }
CriticBrowser class >> criticsBrowserMenuOn: aBuilder [ 
	"I build a menu"
	<worldMenu>
	(aBuilder item: 'Critic Browser')
		action: [ self openOnCurrentWorkingConfiguration];
		order: 0.41;
		parent: #Tools;
		help: 'To manage rule checks.';
		icon: self icon
]

{ #category : #specs }
CriticBrowser class >> defaultSpec [
	<spec>
	
	^ SpecLayout composed 
		newColumn: [:c |
			c 
				newRow: [:r |
					r 
						newColumn: [:c1 |
							c1 add: #rulesModel.
							c1 newRow: [:c2 |
								c2 add: #resetButton.
								c2 add: #logButton
							] height: 25
						];
						newColumn: [:c1 |
							c1 
								add: #(criticModel textInputFieldModel) height: 20;
								add: #(criticModel criticsModel);
								add: #(criticModel toolbarModel) height: 25 ]];
				addSplitter;
				add: #(criticModel textModel) ]
]

{ #category : #icons }
CriticBrowser class >> icon [
	"Answer an icon for the receiver."

	^ self iconNamed: #smallWarningIcon
]

{ #category : #'instance creation' }
CriticBrowser class >> open [

	| env rules  |
	rules :=  RBCompositeLintRule allGoodRules rules.
	env := RBBrowserEnvironment default.
	self openOnRule: rules onEnvironment:  env
]

{ #category : #menu }
CriticBrowser class >> openOnCurrentWorkingConfiguration [

	CriticWorkingConfiguration exists
		ifTrue: [  ResetWindow new openWithSpec  ]
		ifFalse: [ SelectPackageBrowser open]
]

{ #category : #'instance creation' }
CriticBrowser class >> openOnRule: aRule onEnvironment: aEnv [

	| cbr |
	cbr :=  self new rules: aRule;
		environment: aEnv;
		removeTestCase: false;
		yourself.

	cbr openWithSpec.
	cbr applyRules. 
	cbr rulesModel  changed:  #listElementAt:.
]

{ #category : #'instance creation' }
CriticBrowser class >> openOnWorkingConfiguration: aWorkingConfiguration [

	| cbr |
	cbr :=  self new 
		rules: aWorkingConfiguration rule;
		environment: aWorkingConfiguration environment;
		removeTestCase: aWorkingConfiguration removeTestCase;
		yourself.

	cbr openWithSpec.
	cbr applyRules. 
	cbr rulesModel  changed:  #listElementAt:.
]

{ #category : #'instance creation' }
CriticBrowser class >> packagesMenu: aBuilder [
	<nautilusGlobalPackageMenu>
	(aBuilder item: #'Critic Browser')
		icon: self icon;
		action: [
			self  
				openOnRule: (ReRuleManager uniqueInstance allRules) 
				onEnvironment: 
					(RBPackageEnvironment new 
						packages: aBuilder model selectedPackages;
						yourself) ];
		help: 'Running critics rules on this package'
]

{ #category : #icons }
CriticBrowser class >> taskbarIconName [

	^#smallWarningIcon
]

{ #category : #menu }
CriticBrowser class >> theme [
	^ Smalltalk ui theme 
]

{ #category : #'user interface' }
CriticBrowser >> addModelItemsToWindowMenu: aMenu [
	"Add model-related items to the window menu"

	aMenu
		addLine;
		add: 'Clean all manifest' target: checker selector: #cleanAllManifest;
		add: 'Reapply all  rules' target: self selector: #reapplyAllRules
]

{ #category : #private }
CriticBrowser >> addRuleToFalsePositive [
	rulesModel selectedItem ifNotNil: [ :rule |
		rule leaves do: [ :r |
			rbEnvironment packages do: [ :package |
				cache addFalsePositiveRule: r forPackage: package ]]].
	
]

{ #category : #private }
CriticBrowser >> allRules [
	^ rulesModel roots flatCollect: #rules
]

{ #category : #private }
CriticBrowser >> applyRules [
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
	self updateTree.
	self registerToAnnouncements ] newProcess.
	process name: 'SmallLint'.
	process resume
]

{ #category : #private }
CriticBrowser >> browseRule [

	rulesModel selectedItem ifNotNil: [ :item | item content browse ]
]

{ #category : #'system annoucements' }
CriticBrowser >> classAdded: aClass [

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
CriticBrowser >> classRemoved: aClass [

	(rbEnvironment definesClass: aClass)
		ifTrue: [ cache itemRemoved: aClass ].
		
	cache updateBrowser
]

{ #category : #private }
CriticBrowser >> colorForRule: aRule [
	| total |
	^ (total := cache criticsOf: aRule)
		ifEmpty: [ self class theme textColor ]
		ifNotEmpty: [ (cache falsePositiveOf: aRule) = total
				ifTrue: [ criticModel falsePositiveColor ]
				ifFalse: [ criticModel defaultColor ] ]
]

{ #category : #accessing }
CriticBrowser >> criticModel [
	^ criticModel
]

{ #category : #api }
CriticBrowser >> defaultTitle [

	^ 'Critic Browser'
]

{ #category : #accessing }
CriticBrowser >> environment [
	^ rbEnvironment

]

{ #category : #accessing }
CriticBrowser >> environment: aEnv [
	rbEnvironment := aEnv

]

{ #category : #'system annoucements' }
CriticBrowser >> handleClassAdded: anAnnouncement [

	self classAdded: anAnnouncement classAdded

]

{ #category : #'system annoucements' }
CriticBrowser >> handleClassModified: anAnnouncement [

	self
		classRemoved: anAnnouncement oldClassDefinition;
		classAdded:   anAnnouncement newClassDefinition.
	
]

{ #category : #'system annoucements' }
CriticBrowser >> handleClassRemoved: anAnnouncement [

	self classRemoved: anAnnouncement classRemoved

]

{ #category : #'system annoucements' }
CriticBrowser >> handleMethodAdded: anAnnouncement [

	self methodAdded: anAnnouncement methodAdded

]

{ #category : #'system annoucements' }
CriticBrowser >> handleMethodModified: anAnnouncement [

	self
		methodRemoved: anAnnouncement oldMethod;
		methodAdded: anAnnouncement newMethod
	

]

{ #category : #'system annoucements' }
CriticBrowser >> handleMethodRemoved: anAnnouncement [

	self methodRemoved: anAnnouncement methodRemoved

]

{ #category : #api }
CriticBrowser >> initialExtent [

	^   760@370
]

{ #category : #initialization }
CriticBrowser >> initialize [
	
	super initialize.
	cache := ReCriticsCache new.
	checker := ReSmalllintChecker new.
	cache checker: checker.
	cache browser: self.
	criticModel cache: cache.
]

{ #category : #initialization }
CriticBrowser >> initializePresenter [

	rulesModel whenSelectedItemChanged: [ :rule |
		(rule isNil or: [ rule isComposite ]) ifFalse: [ 
				criticModel 
					resetSelection;
	 				rule: rule;	
					setTextModelForNil
			].
				
			self setTitle: ( 
				rule 
					ifNil: [ self defaultTitle  ] 
					ifNotNil: [ rule name ]
			)
			
	].


]

{ #category : #initialization }
CriticBrowser >> initializeWidgets [
	
	title := self defaultTitle.
	rulesModel := self newTree.
	resetButton := self newButton.
	logButton := self newButton.
	
	self setLogButton.
	self setResetButton.
	 
	criticModel := SingleCodeCriticResultList new.
	rulesModel childrenBlock: [ :rule | 
		rule isComposite 
			ifTrue: [ rule rules ]
			ifFalse: [ #() ]]. 

	rulesModel displayBlock: [ :rule | self stringMorphForRule: rule ].
	
	rulesModel menu: [ :aMenu :shifted | self menu: aMenu shifted: shifted ].	
	
	self focusOrder
		add: rulesModel;
		add: criticModel
]

{ #category : #accessing }
CriticBrowser >> logButton [
	^ logButton 
]

{ #category : #private }
CriticBrowser >> logInManifest [
	cache logInManifest
]

{ #category : #menu }
CriticBrowser >> menu: aMenu shifted: aBoolean [
	aMenu
		addGroup:[ :group | 
			group addItem: [ :item |
				item 
					name: 'Browse rule' translated;
					action: [ self browseRule ]].
			group addItem: [ :item |
				item 
					name: 'Reapply this rule' translated;
					action: [ self reapplyThisRule ] ] ];
		addGroup:[ :group | 
			group addItem: [ :item |
				item 
					name: 'Ban from selected packages' translated;
					action: [ self addRuleToFalsePositive ]].
			group addItem: [ :item |
				item 
					name: 'Unban from selected packages' translated;
					action: [ self removeRuleToFalsePositive ] ] ].
		
	 ^ aMenu
]

{ #category : #'system annoucements' }
CriticBrowser >> methodAdded: aMethod [

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
CriticBrowser >> methodRemoved: aMethod [

	(rbEnvironment includesMethod: aMethod)
		ifTrue: [ cache itemRemoved: aMethod ].
		
	cache updateBrowser
]

{ #category : #api }
CriticBrowser >> onWindowClosed [

	self unregisterFromAnnouncements.

	cache cacheChange 
		ifTrue: [	
			(UIManager default confirm: 'Do you want log all wrong violations in the Manifests
before closing the Critics Browser ?')
				ifTrue: [ cache logInManifest ]].
	

]

{ #category : #display }
CriticBrowser >> open [

	^ self openWithSpec
]

{ #category : #private }
CriticBrowser >> reapplyAllRules [
	
	cache initialize.
	self applyRules 
]

{ #category : #private }
CriticBrowser >> reapplyRule: aRule [

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
CriticBrowser >> reapplyThisRule [

	| rule |
	rulesModel selectedItem ifNil: [ ^ self ].
	rule := rulesModel selectedItem content.
	rule isComposite 
		ifTrue: [ rule rules do: [ :each | self reapplyRule: each ] ]
		ifFalse: [ self reapplyRule: rule ]
	

]

{ #category : #'system annoucements' }
CriticBrowser >> registerToAnnouncements [
	
	self unregisterFromAnnouncements.
	
	SystemAnnouncer uniqueInstance weak
		when: ClassAdded send: #handleClassAdded: to: self;
		when: ClassModifiedClassDefinition send: #handleClassModified: to: self;
		when: ClassRemoved send: #handleClassRemoved: to: self;
		when: MethodAdded send: #handleMethodAdded: to: self;
		when: MethodModified send: #handleMethodModified: to: self;
		when: MethodRemoved send: #handleMethodRemoved: to: self.
	self window window announcer when: WindowClosed send: #onWindowClosed to: self.	
]

{ #category : #private }
CriticBrowser >> removeRuleToFalsePositive [

	rulesModel selectedItem ifNotNil: [ :rule |
		rule leaves do: [ :r |
			rbEnvironment packages do: [ :package |
				 cache removeFalsePositiveRule: r forPackage: package ]]].

	
	
]

{ #category : #accessing }
CriticBrowser >> removeTestCase: aBoolean [
	removeTestCase :=  aBoolean
]

{ #category : #accessing }
CriticBrowser >> resetButton [
	^ resetButton 
]

{ #category : #accessing }
CriticBrowser >> rules: rulesCollection [

	self rulesModel roots: ((
		(rulesCollection groupedBy: #group) associations collect: [ :as |
			CriticBrowserRulesGroup
				named: as key
				rules: as value ])
					sorted: [ :a :b | a name < b name ])
]

{ #category : #accessing }
CriticBrowser >> rulesModel [

	^ rulesModel
]

{ #category : #initialization }
CriticBrowser >> setActionLogButtom [
	
	^ [(UIManager default confirm:  'Do you want to save all false positive and toDo in the Manifests ?
(this action may generate new manifest classes and make dirty your package)')
			ifTrue: [ cache logInManifest] ]
]

{ #category : #initialization }
CriticBrowser >> setActionResetButtom [
	
	^ [(UIManager default confirm: 'Do you want to delete the current configuration
 and create a new configuration ?')
		ifTrue: [ self  delete. 
				SelectPackageBrowser open.] ]
]

{ #category : #initialization }
CriticBrowser >> setLogButton [

	logButton 
		state: false;
		label: 'Save Critics';
		action: self setActionLogButtom 
]

{ #category : #initialization }
CriticBrowser >> setResetButton [

	resetButton 
		state: false;
		label: 'Run new configuration';
		action: self setActionResetButtom.
]

{ #category : #api }
CriticBrowser >> setTitle: aTitle [

	title := aTitle.
	
	self window updateTitle 
	
]

{ #category : #private }
CriticBrowser >> stringMorphForRule: rule [
	| unclassified falsePositives toDos text total |
	falsePositives := (cache falsePositiveOf: rule) size.
	toDos := (cache toDosOf: rule) size.
	total := (cache criticsOf: rule) size.
	unclassified := total - falsePositives - toDos.
	text := String
		streamContents: [ :s | 
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

{ #category : #api }
CriticBrowser >> title [
	^ title
]

{ #category : #'system annoucements' }
CriticBrowser >> unregisterFromAnnouncements [

	SystemAnnouncer uniqueInstance unsubscribe: self
]

{ #category : #thread }
CriticBrowser >> updateTree [
	criticModel updateList. 
	rulesModel updateTree
]

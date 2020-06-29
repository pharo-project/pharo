"
A RewriteBrowserPresenter is a tool that provides GUI for applying rewrite rule on certain scope of classes. If you select a package, all of their classes will be added to the scope. If you select a class, the scope will contain only that class. Multiple selection in suported.

To open:
RewriteBrowserPresenter open
"
Class {
	#name : #RewriteRuleApplierPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'rules',
		'packages',
		'classes',
		'methods',
		'environment',
		'checkbox',
		'buttonBar'
	],
	#category : #'RewriteRuleEditor-Applier'
}

{ #category : #accessing }
RewriteRuleApplierPresenter class >> buttonHeight [
	^ 40
]

{ #category : #accessing }
RewriteRuleApplierPresenter class >> buttonWidth [
	^ 150
]

{ #category : #specs }
RewriteRuleApplierPresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add:
			(SpBoxLayout newHorizontal
				add: #packages;
				add: #classes;
				add: #methods;
				yourself);
		addLast:
			(SpBoxLayout newHorizontal
				add: #checkbox expand: false;
				add: #rules expand: false;
				add: #buttonBar;
				yourself);
		yourself
]

{ #category : #accessing }
RewriteRuleApplierPresenter class >> icon [
	^ self iconNamed: #objects
]

{ #category : #api }
RewriteRuleApplierPresenter class >> open [
	<script>
	^ self new openWithSpec
]

{ #category : #accessing }
RewriteRuleApplierPresenter class >> rulesWidth [
	^ 225
]

{ #category : #accessing }
RewriteRuleApplierPresenter >> allRules [
	
	^ OrderedCollection new
		addAll: self demoRules;
		addAll: self customRules;
		yourself.
	
]

{ #category : #testing }
RewriteRuleApplierPresenter >> areSelectedItemsValid: items [
	^ (items allSatisfy: [ :class | class isNotNil ])
		and: [ items isNotEmpty ]
]

{ #category : #accessing }
RewriteRuleApplierPresenter >> basicEditorIcon [
	^ RewriteBasicEditorPresenter icon
]

{ #category : #actions }
RewriteRuleApplierPresenter >> browseRule [
	Smalltalk tools browser
		openOnClass: (Smalltalk globals at: rules selectedItem)
]

{ #category : #actions }
RewriteRuleApplierPresenter >> classesChanged [
	| methodsInClasses |
	(self areSelectedItemsValid: classes selectedItems)
		ifFalse: [ ^ self ].
	methodsInClasses := OrderedCollection new.
	environment := classes selectedItems.
	classes selectedItems
		do: [ :eachClass | methodsInClasses addAll: eachClass selectors ].
	methods items: methodsInClasses.
	methods resetListSelection
]

{ #category : #initialization }
RewriteRuleApplierPresenter >> connectPresenters [
	packages whenSelectionChangedDo: [ self packagesChanged ].
	classes whenSelectionChangedDo: [ self classesChanged ].
	checkbox
		whenActivatedDo: [ rules items: self customRules ];
		whenDeactivatedDo: [ rules items: self allRules ]
]

{ #category : #accessing }
RewriteRuleApplierPresenter >> customRules [
	^ (RBCustomTransformationRule subclasses
		collect: [ :each | each name ] )asOrderedCollection 
]

{ #category : #accessing }
RewriteRuleApplierPresenter >> demoRules [
	^ (RBTransformationRule subclasses collect: [ :each | each name ])
		asOrderedCollection
]

{ #category : #initialization }
RewriteRuleApplierPresenter >> initializeButtonBar [
	buttonBar := self newButtonBar
		add:
			(self newButton
				label: 'Apply rule';
				icon: (self iconNamed: #smallOk);
				action: [ self runReplace ];
				yourself);
		add:
			(self newButton
				label: 'Browse rule';
				icon: (self iconNamed: #smallSystemBrowser);
				action: [ self browseRule ];
				yourself);
		add:
			(self newButton
				label: 'Edit rule';
				icon: self basicEditorIcon;
				action: [ self openRuleInBasicEditor.
					self window close ];
				yourself);
		yourself
]

{ #category : #initialization }
RewriteRuleApplierPresenter >> initializeFocus [
	self focusOrder
		add: packages;
		add: classes;
		add: methods;
		add: checkbox;
		add: rules;
		add: buttonBar
]

{ #category : #initialization }
RewriteRuleApplierPresenter >> initializePresenters [
	packages := self newList
		items: RBBrowserEnvironment new packages;
		headerTitle: 'Packages';
		display: [ :item | item name ];
		icons: [ self iconNamed: #packageIcon ];
		sortingBlock: [ :a :b | a name < b name ];
		enableItemSubstringFilter;
		beMultipleSelection;
		yourself.
	classes := self newList
		headerTitle: 'Classes';
		display: [ :item | item name ];
		icons: [ :elem | elem systemIcon ];
		sortingBlock: [ :a :b | a name < b name ];
		enableItemSubstringFilter;
		beMultipleSelection;
		yourself.
	methods := self newList
		headerTitle: 'Methods';
		display: [ :item | '    ' , item ];
		enableItemSubstringFilter;
		yourself.
	rules := self newDropList
		display: [ :item | item asString ];
		items: self allRules;
		yourself.
	checkbox := self newCheckBox
		label: 'Only my rules';
		yourself.
	environment := OrderedCollection new.
	self initializeButtonBar.
	self initializeFocus
]

{ #category : #initialization }
RewriteRuleApplierPresenter >> initializeWindow: aWindowPresenter [
	aWindowPresenter
		title: 'Rewrite Rule Applier' translated;
		initialExtent: 750 @ 500;
		windowIcon: (self class icon);
		askOkToClose: false;
		aboutText: 'Apply your custom rewrite rules to packages or classes.'
]

{ #category : #actions }
RewriteRuleApplierPresenter >> openRuleInBasicEditor [
	| aRule editor |
	aRule := (self class environment at: rules selectedItem) new.
	editor := RewriteBasicEditorPresenter new.
	editor searchForCode: aRule searchFor.
	editor replaceWithCode: aRule replaceWith.
	editor openWithSpec
]

{ #category : #actions }
RewriteRuleApplierPresenter >> openRuleInExampleBasedEditor [
	| aRule builder |
	self
		flag: 'Not used method. It will modified to work on later versions'.
	aRule := (self class environment at: rules selectedItem) new.
	builder := RewriteRuleBuilderPresenter new.
	builder sourcePanel codeSource: aRule input.
	builder resultPanel codeSource: aRule output.
	builder transformationRule searchForPanel codeSource: aRule searchFor.
	builder transformationRule replaceWithPanel
		codeSource: aRule replaceWith.
	builder transformationRule ruleName: aRule class asString.
	builder transformationRule packageName: aRule class category asString.
	builder openWithSpec
]

{ #category : #actions }
RewriteRuleApplierPresenter >> packagesChanged [
	(self areSelectedItemsValid: packages selectedItems)
		ifFalse: [ ^ self ].
	environment removeAll.
	packages selectedItems
		do: [ :each | environment addAll: each classes ].
	classes items: environment.
	classes resetListSelection
]

{ #category : #actions }
RewriteRuleApplierPresenter >> runReplace [
	| changes selectedRule |
	selectedRule := (self class environment at: rules selectedItem) new.
	changes := Array
		with:
			(RBSmalllintChecker
				runRule: selectedRule
				onEnvironment: (RBClassEnvironment classes: environment)) builder.
	(RewriteRuleChangesBrowser changes: changes) open
	"changes := RBSmalllintChecker
		runRule: selectedRule
		onEnvironment: (RBClassEnvironment classes: environment).
	(ChangesBrowser changes: changes builder changes) open"
]

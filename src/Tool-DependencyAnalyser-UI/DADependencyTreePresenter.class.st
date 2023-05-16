"
A PDPackageAnalyzerTreeModel shows all dependent packages from a set of packages. Also you can see all type of the dependency, and browse them.
"
Class {
	#name : #DADependencyTreePresenter,
	#superclass : #DAPackageTreePresenter,
	#instVars : [
		'systemCycles',
		'packageLabel',
		'buttonBrowseCycles',
		'buttonDefault',
		'textPackageField',
		'filter',
		'buttonAddPackage',
		'buttonGenerateGraphViz',
		'buttonRemovePackage',
		'buttonReverseAnalysis',
		'buttonSave',
		'buttonMakeDiff',
		'relationGraph'
	],
	#category : #'Tool-DependencyAnalyser-UI-Core'
}

{ #category : #layout }
DADependencyTreePresenter class >> defaultLayout [
	<spec: #default>
	^ SpBoxLayout newVertical
		add: ( SpBoxLayout newHorizontal
					add: #textPackageField;
					add: #buttonBrowseCycles;
					add: #buttonRefresh;
					add: #buttonDefault;
					yourself)
			expand: false;
		add: #packageLabel expand: false;
		add: #tree;
		add: (SpBoxLayout newHorizontal
				add: #buttonAddPackage;
				add: #buttonRemovePackage;
				add: #buttonReverseAnalysis;
				yourself)
			expand: false;
		yourself
]

{ #category : #'instance creation' }
DADependencyTreePresenter class >> onPackages: aCollection [
	^ self new
		initializeWithRPackageSet: aCollection;
		yourself
]

{ #category : #examples }
DADependencyTreePresenter class >> onPackagesMatch: match [

	^ self onPackagesNamed: (Smalltalk organization packages
			   select: [ :package | match match: package packageName asString ]
			   thenCollect: [ :package | package packageName ])
]

{ #category : #'instance creation' }
DADependencyTreePresenter class >> onPackagesNamed: aCollection [
	^ self onPackages: (aCollection collect: [ :each | RPackageSet named: each ])
]

{ #category : #protocol }
DADependencyTreePresenter >> actionOnAddPackage [
	buttonAddPackage action: [ (DAAddPackagePresenter
					new
					dependenciesPresenter: self)
					open ]
]

{ #category : #protocol }
DADependencyTreePresenter >> actionOnBrowseCycles [
	buttonBrowseCycles action: [ self actionOnFindCycles ]
]

{ #category : #protocol }
DADependencyTreePresenter >> actionOnButtons [

	self
		actionOnBrowseCycles;
		actionWhenTextFieldChanged;
		actionOnDefaultSettings;
		actionOnRemovePackage;
		actionOnAddPackage;
		 actionOnGenerateGraphViz;
		actionOnReverseAnalysis;
		actionOnSave
]

{ #category : #protocol }
DADependencyTreePresenter >> actionOnDefaultSettings [
	buttonDefault
		action: [
			self resetDefaultSettings.
			self buildRoots ]
]

{ #category : #protocol }
DADependencyTreePresenter >> actionOnFindCycles [

	| selectedPackages selectedItems |
	selectedItems := self selectedItemsFromTree.
	(self arePackageNodes: selectedItems) ifFalse: [ ^ self ].

	self notify: 'This could take a while, confirm?'.
	selectedPackages := self pdPackagesFrom: selectedItems.
	systemCycles ifNil: [ systemCycles := DACycleDetectionPresenter system ].
	systemCycles
		updateWithFilter: selectedPackages;
		open
]

{ #category : #protocol }
DADependencyTreePresenter >> actionOnGenerateGraphViz [
	| graphvizGenerator |
	buttonGenerateGraphViz
		action: [
			self ifGraphVizAbsent: [ self error: 'Please install Graphviz Support' ].
			graphvizGenerator := DAPackageRelationGraphGenerator newWith: self relationGraph named: 'PackageDependencyGraph'.
			self saveGraphAndOpenWith: graphvizGenerator ]
]

{ #category : #protocol }
DADependencyTreePresenter >> actionOnRemovePackage [
	| packagesToRemove |
	buttonRemovePackage
		action: [
			self selectedItemsFromTree
				ifNotEmpty: [
					packagesToRemove := self selectedItemsFromTree.
					packagesToRemove do: [ :item | self relationGraph removePackage: item content packageName ].
					self buildRoots ] ]
]

{ #category : #protocol }
DADependencyTreePresenter >> actionOnReverseAnalysis [

	buttonReverseAnalysis action: [
		(DAPackageDependenciesPresenter on:
			 (DAReverseTreePresenter onRelationGraph: self relationGraph)) open ]
]

{ #category : #protocol }
DADependencyTreePresenter >> actionOnSave [
	buttonSave
		action: [ self saveCurrentRelationGraph ]
]

{ #category : #protocol }
DADependencyTreePresenter >> actionWhenTextFieldChanged [
	textPackageField
		whenTextChangedDo: [ textPackageField text
				ifEmpty: [ self resetFilter ]
				ifNotEmpty: [ self filter: textPackageField text ].
			self buildRoots ]
]

{ #category : #actions }
DADependencyTreePresenter >> buildRoots [
	| matching roots |
	roots := self filter
		ifNotNil: [ :f |
			matching := '*' , f , '*'.
			self nodesFor: (relationGraph seenPackagesWithFilter: matching) ]
		ifNil: [ self nodesFor: relationGraph seenPackages ].
	self tree roots: roots.
	self updatePackageLabel
]

{ #category : #initialization }
DADependencyTreePresenter >> connectPresenters [
	super connectPresenters.
	self actionOnButtons.
	tree
		whenSelectedItemChangedDo: [ :item |
			item
				ifNil: [ buttonBrowseCycles enabled: false ]
				ifNotNil: [
					buttonBrowseCycles enabled: true.
					tree contextMenu: [ self loadMenuForSelectedItem ].
					self tree selectedItem selectInBrowser ] ]
]

{ #category : #saving }
DADependencyTreePresenter >> createDirectoryForSaving [
	| dir |
	dir := FileSystem workingDirectory / self defaultDirectory.
	dir ensureCreateDirectory
]

{ #category : #saving }
DADependencyTreePresenter >> defaultDirectory [
	^ 'saving'
]

{ #category : #saving }
DADependencyTreePresenter >> defaultName [
	^ self defaultDirectory, '/relationGraph.FL'
]

{ #category : #protocol }
DADependencyTreePresenter >> extent [
	^ 700@500
]

{ #category : #accessing }
DADependencyTreePresenter >> filter [
	^ filter
]

{ #category : #accessing }
DADependencyTreePresenter >> filter: aString [
	filter := aString
]

{ #category : #initialization }
DADependencyTreePresenter >> initializeButtons [
	buttonBrowseCycles := self newButton
		help: 'Find all the cycles where the package is in the system';
		label: 'Find cycles'.
	buttonDefault := self newButton
		help: 'Back to the default settings';
		label: 'Default settings'.
	buttonAddPackage := self newButton
		help: 'Add package to the current analysis';
		label: 'Add packages';
		icon: (Smalltalk ui icons iconNamed: #addIcon).
	buttonRemovePackage := self newButton
		help: 'Remove package to the current analysis';
		label: 'Remove packages';
		icon: (self iconNamed: #removeIcon).
	buttonGenerateGraphViz := self newButton
		help: 'Open the graph in world';
		label: 'Open the graph'.
	buttonReverseAnalysis := self newButton
		help: 'Reverse the analysis : set the dependent packages as root packages';
		label: 'Reverse the analysis'.
	buttonSave := self newButton
		help: 'Serialize the current object relation graph dependencies';
		label: 'Save current analysis'.
	buttonMakeDiff := self newButton
		label: 'Make diff'
]

{ #category : #initialization }
DADependencyTreePresenter >> initializePresenters [
	super initializePresenters.
	packageLabel := self newLabel label: 'Analysis of packages'.
	textPackageField := self newTextInput
		placeholder: 'Enter a package name';
		entryCompletion: self packagesEntryCompletion.
	self initializeButtons
]

{ #category : #accessing }
DADependencyTreePresenter >> initializeWithRPackageSet: aCollection [
	self relationGraph: (DAPackageRelationGraph onPackages: (aCollection collect: [ :each | DAPackage on: each ]))
]

{ #category : #private }
DADependencyTreePresenter >> nodesFor: anItemList [
	^ anItemList sorted collect:
		[ :each |
		DAPackageUnderAnalysisNode new
			content: each;
			cycle: false;
			treeModelParent: self;
			browser: self browser;
			yourself ]
]

{ #category : #accessing }
DADependencyTreePresenter >> packagesEntryCompletion [
	| applicants |
	applicants := (Smalltalk organization packages collect: [ :package | package packageName asString ]).

	^ EntryCompletion new
				dataSourceBlock: [:currText | applicants];
				filterBlock: [:currApplicant :currText | currText size > 3
						and: [currApplicant asUppercase includesSubstring: currText asString asUppercase]]
]

{ #category : #accessing }
DADependencyTreePresenter >> pdPackagesFrom: selectedItems [
	^ selectedItems collect: [ :item | DAPackage on: (RPackageSet named: item content packageName asString) ]
]

{ #category : #actions }
DADependencyTreePresenter >> refresh [
	self relationGraph refreshGraph.
	self buildRoots
]

{ #category : #accessing }
DADependencyTreePresenter >> relationGraph [
	^ relationGraph
]

{ #category : #accessing }
DADependencyTreePresenter >> relationGraph: anObject [
	| warnings alertMessage |
	warnings := Set new.
	relationGraph := anObject.

	[ self relationGraph build ]
		on: DAPotentialOutDatedDependencyWarning
		do: [ :ex | warnings add: ex. ex resume ].
	self buildRoots.
	alertMessage := 'Some dependencies declared in ' ,
		self relationGraph packagesName first,
		' Manifest as manuallyResolvedDependencies not detected as a dependency! Please check :' , String cr.
	warnings 	ifNotEmpty: [
		UIManager default
			alert: alertMessage , (String cr join: (warnings collect: #dependency ))
			title: 'Potential outdated dependencies' ]
]

{ #category : #actions }
DADependencyTreePresenter >> resetDefaultSettings [
	self resetFilter.
	textPackageField text: ''
]

{ #category : #actions }
DADependencyTreePresenter >> resetFilter [
	^ self filter: nil
]

{ #category : #saving }
DADependencyTreePresenter >> saveCurrentRelationGraph [
	self createDirectoryForSaving.
	^  FLSerializer new
			filePath: self defaultName;
			object: relationGraph;
			serialize
]

{ #category : #accessing }
DADependencyTreePresenter >> seenPackagesName [
	^ self relationGraph seenPackagesName
]

{ #category : #accessing }
DADependencyTreePresenter >> sizeOfRoots [
	^ self treeRoots size
]

{ #category : #protocol }
DADependencyTreePresenter >> title [
	^ 'Dependencies analysis'
]

{ #category : #accessing }
DADependencyTreePresenter >> treeRoots [
	^ self tree roots
]

{ #category : #accessing }
DADependencyTreePresenter >> updatePackageLabel [
	packageLabel label: 'Analysis of ' , self sizeOfRoots asString , ' package(s)'
]

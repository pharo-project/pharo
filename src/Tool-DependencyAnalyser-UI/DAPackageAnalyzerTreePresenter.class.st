"
A PDPackageAnalyzerTreeModel shows all dependent packages from a set of packages. Also you can see all type of the dependency, and browse them.
"
Class {
	#name : #DAPackageAnalyzerTreePresenter,
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

{ #category : #specs }
DAPackageAnalyzerTreePresenter class >> defaultSpec [
	<spec: #default>
	^ SpecLayout composed
		newColumn:
			[ :col | 
			col
				newRow: [ :row | 
							row
								add: #textPackageField;
								add: #buttonBrowseCycles;
								add: #buttonRefresh;
								add: #buttonDefault
								 ]
					height: 30;
				newRow: [ :row | 
							row
								add: #packageLabel ]
					height: 20;
				add: #tree;
				newRow: [ :row | 
							row
								add: #buttonAddPackage;
								add: #buttonRemovePackage;
								"add: #buttonGenerateGraphViz;"
								"add: #buttonMakeDiff;"
								add: #buttonReverseAnalysis
								 ]
					height: 30 ]	
					  yourself
]

{ #category : #'instance creation' }
DAPackageAnalyzerTreePresenter class >> onPackages: aCollection [
	^ self new
		initializeWithRPackageSet: aCollection;
		yourself
]

{ #category : #examples }
DAPackageAnalyzerTreePresenter class >> onPackagesMatch: match [
	^ (self onPackagesNamed:
			(RPackageOrganizer default packages
				select: [ :package | match match: package packageName asString ]
				thenCollect: [ :package | package packageName ]) )
]

{ #category : #'instance creation' }
DAPackageAnalyzerTreePresenter class >> onPackagesNamed: aCollection [
	^ self onPackages: (aCollection collect: [ :each | RPackageSet named: each ])
]

{ #category : #protocol }
DAPackageAnalyzerTreePresenter >> actionOnAddPackage [
	self buttonAddPackage action: [ (DAAddPackagePresenter 
					new 
					packageAnalyzerTreeModel: self)
					open ]
]

{ #category : #protocol }
DAPackageAnalyzerTreePresenter >> actionOnBrowseCycles [
	buttonBrowseCycles action: [ self actionOnFindCycles ]
]

{ #category : #protocol }
DAPackageAnalyzerTreePresenter >> actionOnButtons [

	self
		actionOnBrowseCycles;
		actionWhenTextFieldChanged;
		actionOnDefaultSettings;
		actionOnRemovePackage;
		actionOnAddPackage;
		 actionOnGenerateGraphViz;
		actionOnReverseAnalysis;
		actionOnSave.
]

{ #category : #protocol }
DAPackageAnalyzerTreePresenter >> actionOnDefaultSettings [
	self buttonDefault
		action: [ 
			self resetDefaultSettings.
			self buildRoots ]
]

{ #category : #protocol }
DAPackageAnalyzerTreePresenter >> actionOnFindCycles [
	| selectedPackages selectedItems |
	selectedItems := self selectedItemsFromTree.
	(self arePackageNodes: selectedItems)
		ifTrue: [ 
			self notify: 'This could take a while, confirm?'.
			selectedPackages := self pdPackagesFrom: selectedItems.
			systemCycles ifNil: [ systemCycles := DAPackageAnalyzerCycleDetectionWindow  system ].
			systemCycles
				updateWithFilter: selectedPackages;
				open ]
]

{ #category : #protocol }
DAPackageAnalyzerTreePresenter >> actionOnGenerateGraphViz [
	| graphvizGenerator |
	buttonGenerateGraphViz
		action: [  
			self ifGraphVizAbsent: [ self error: 'Please install Graphviz Support' ].
			graphvizGenerator := DAPackageRelationGraphGenerator newWith: self relationGraph named: 'PackageDependencyGraph'.
			self saveGraphAndOpenWith: graphvizGenerator ]
]

{ #category : #protocol }
DAPackageAnalyzerTreePresenter >> actionOnRemovePackage [
	| packagesToRemove |
	self buttonRemovePackage
		action: [ 
			self selectedItemsFromTree
				ifNotEmpty: [ 
					packagesToRemove := self selectedItemsFromTree.
					packagesToRemove do: [ :item | self relationGraph removePackage: item content packageName ].
					self buildRoots ] ]
]

{ #category : #protocol }
DAPackageAnalyzerTreePresenter >> actionOnReverseAnalysis [
	buttonReverseAnalysis action: [ 
		(DAPackageAnalyzerPackageDependenciesWindow new 
						initializeWithRelationGraph: self relationGraph) 
					open ]
]

{ #category : #protocol }
DAPackageAnalyzerTreePresenter >> actionOnSave [
	self buttonSave 
		action: [ self saveCurrentRelationGraph ]
]

{ #category : #protocol }
DAPackageAnalyzerTreePresenter >> actionWhenTextFieldChanged [
	textPackageField
		whenTextChanged: [ textPackageField text
				ifEmpty: [ self resetFilter ]
				ifNotEmpty: [ self filter: textPackageField text ].
			self buildRoots ]
]

{ #category : #actions }
DAPackageAnalyzerTreePresenter >> buildRoots [
	| matching roots |
	roots := self filter
		ifNotNil: [ :f | 
			matching := '*' , f , '*'.
			self nodesFor: (relationGraph seenPackagesWithFilter: matching) ]
		ifNil: [ self nodesFor: relationGraph seenPackages ].
	self tree roots: roots.
	self updatePackageLabel
]

{ #category : #accessing }
DAPackageAnalyzerTreePresenter >> buttonAddPackage [
	^ buttonAddPackage
]

{ #category : #accessing }
DAPackageAnalyzerTreePresenter >> buttonBrowseCycles [
	^ buttonBrowseCycles
]

{ #category : #accessing }
DAPackageAnalyzerTreePresenter >> buttonDefault [
	^ buttonDefault
]

{ #category : #accessing }
DAPackageAnalyzerTreePresenter >> buttonGenerateGraphViz [
	^ buttonGenerateGraphViz
]

{ #category : #accessing }
DAPackageAnalyzerTreePresenter >> buttonMakeDiff [
	^ buttonMakeDiff
]

{ #category : #accessing }
DAPackageAnalyzerTreePresenter >> buttonRemovePackage [
	^ buttonRemovePackage
]

{ #category : #accessing }
DAPackageAnalyzerTreePresenter >> buttonReverseAnalysis [
	^ buttonReverseAnalysis
]

{ #category : #accessing }
DAPackageAnalyzerTreePresenter >> buttonSave [
	^ buttonSave
]

{ #category : #saving }
DAPackageAnalyzerTreePresenter >> createDirectoryForSaving [
	| dir |
	dir := FileSystem workingDirectory / self defaultDirectory.
	dir ensureCreateDirectory
]

{ #category : #saving }
DAPackageAnalyzerTreePresenter >> defaultDirectory [
	^ 'saving'
]

{ #category : #saving }
DAPackageAnalyzerTreePresenter >> defaultName [
	^ self defaultDirectory, '/relationGraph.FL'
]

{ #category : #protocol }
DAPackageAnalyzerTreePresenter >> extent [
	^ 700@500
]

{ #category : #accessing }
DAPackageAnalyzerTreePresenter >> filter [
	^ filter
]

{ #category : #accessing }
DAPackageAnalyzerTreePresenter >> filter: aString [
	filter := aString
]

{ #category : #initialization }
DAPackageAnalyzerTreePresenter >> initializeButtons [
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
DAPackageAnalyzerTreePresenter >> initializePresenter [
	super initializePresenter.
	self actionOnButtons.
	tree
		whenSelectedItemChanged: [ :item |
			item
				ifNil: [ buttonBrowseCycles enabled: false ]
				ifNotNil: [ 
					buttonBrowseCycles enabled: true.
					tree menu: [ :aMenu | self loadMenuFor: item in: aMenu ].
					self tree selectedItem selectInBrowser ] ]
]

{ #category : #initialization }
DAPackageAnalyzerTreePresenter >> initializeWidgets [
	super initializeWidgets.
	packageLabel := self newLabel label: 'Analysis of packages'.
	textPackageField := self newTextInput
		ghostText: 'Enter a package name';
		entryCompletion: self packagesEntryCompletion.
	self initializeButtons
]

{ #category : #accessing }
DAPackageAnalyzerTreePresenter >> initializeWithRPackageSet: aCollection [
	self relationGraph: (DAPackageRelationGraph onPackages: (aCollection collect: [ :each | DAPackage on: each ]))
]

{ #category : #private }
DAPackageAnalyzerTreePresenter >> nodesFor: anItemList [ 
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
DAPackageAnalyzerTreePresenter >> packageLabel [
	^ packageLabel
]

{ #category : #accessing }
DAPackageAnalyzerTreePresenter >> packagesEntryCompletion [
	| applicants |
	applicants := (RPackageOrganizer default packages collect: [ :package | package packageName asString ]).

	^ EntryCompletion new
				dataSourceBlock: [:currText | applicants];
				filterBlock: [:currApplicant :currText | currText size > 3
						and: [currApplicant asUppercase includesSubstring: currText asString asUppercase]].
]

{ #category : #accessing }
DAPackageAnalyzerTreePresenter >> pdPackagesFrom: selectedItems [
	^ selectedItems collect: [ :item | DAPackage on: (RPackageSet named: item content packageName asString) ]
]

{ #category : #actions }
DAPackageAnalyzerTreePresenter >> refresh [
	self isOpen ifTrue: [  
			self relationGraph refreshGraph.
			self buildRoots].
		
]

{ #category : #accessing }
DAPackageAnalyzerTreePresenter >> relationGraph [
	^ relationGraph
]

{ #category : #accessing }
DAPackageAnalyzerTreePresenter >> relationGraph: anObject [
	| warnings alertMessage |
	warnings := Set new.
	relationGraph := anObject.

	[ self relationGraph build ]
		on: PotentialOutDatedDependencyWarning 
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

{ #category : #accessing }
DAPackageAnalyzerTreePresenter >> relationGraphOnReverse: anObject [
	relationGraph := anObject.
	self relationGraph build.
	self buildReverseRoots
]

{ #category : #actions }
DAPackageAnalyzerTreePresenter >> resetDefaultSettings [
	self resetFilter.
	self textPackageField text: ''
]

{ #category : #actions }
DAPackageAnalyzerTreePresenter >> resetFilter [
	^ self filter: nil
]

{ #category : #saving }
DAPackageAnalyzerTreePresenter >> saveCurrentRelationGraph [
	self createDirectoryForSaving.
	^  FLSerializer serialize: relationGraph toFileNamed: self defaultName. 
]

{ #category : #accessing }
DAPackageAnalyzerTreePresenter >> seenPackagesName [
	^ self relationGraph seenPackagesName
]

{ #category : #accessing }
DAPackageAnalyzerTreePresenter >> sizeOfRoots [
	^ self treeRoots size
]

{ #category : #accessing }
DAPackageAnalyzerTreePresenter >> textPackageField [
	^ textPackageField
]

{ #category : #protocol }
DAPackageAnalyzerTreePresenter >> title [
	^ 'Dependencies analysis'
]

{ #category : #accessing }
DAPackageAnalyzerTreePresenter >> treeRoots [
	^ self tree roots
]

{ #category : #accessing }
DAPackageAnalyzerTreePresenter >> updatePackageLabel [
	self packageLabel label: 'Analysis of ' , self sizeOfRoots asString , ' package(s)'
]

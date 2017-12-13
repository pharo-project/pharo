"
Browser for working copies
"
Class {
	#name : #MCWorkingCopyBrowser,
	#superclass : #MCTool,
	#instVars : [
		'workingCopy',
		'workingCopyWrapper',
		'repository',
		'defaults',
		'order',
		'repositoryPattern',
		'workingCopyPattern',
		'repositoryProcess',
		'packageProcess',
		'showOnlyRepositoriesFromWorkingCopy'
	],
	#classVars : [
		'Order'
	],
	#classInstVars : [
		'repositorySearchMaxSize',
		'workingCopySearchMaxSize',
		'workingCopySearchList',
		'repositorySearchList'
	],
	#category : #MonticelloGUI
}

{ #category : #'world menu' }
MCWorkingCopyBrowser class >> menuCommandOn: aBuilder [
	<worldMenu>
	(aBuilder item: #'Monticello Browser')
		parent: #MostUsedTools;
		action: [ Smalltalk tools openMonticelloBrowser ];
		order: 0.9;
		keyText: 'o, p';
		help: 'Source code versionning system to manage Smalltalk code.';
		icon: Smalltalk tools monticelloBrowser taskbarIcon
]

{ #category : #'instance creation' }
MCWorkingCopyBrowser class >> open [
	<script>
	self new show
]

{ #category : #settings }
MCWorkingCopyBrowser class >> order [
	"
	anInteger represents the order of the mcworkingcopies in the MCBrowser. Its value is between 0 and the number of entries in MCFileRepositoryInspector >> orderSpecs
	"
	^ Order ifNil: [ Order := 2 ]
]

{ #category : #settings }
MCWorkingCopyBrowser class >> order: anInteger [
	"
	anInteger represents the order of the mcworkingcopies in the MCBrowser. Its value is between 0 and the number of entries in MCFileRepositoryInspector >> orderSpecs
	"
	Order := anInteger
]

{ #category : #'tools registry' }
MCWorkingCopyBrowser class >> registerToolsOn: registry [
	registry register: self as: #monticelloBrowser
]

{ #category : #settings }
MCWorkingCopyBrowser class >> repositorySearchList [
	^ repositorySearchList ifNil: [ repositorySearchList := OrderedCollection new ].
]

{ #category : #settings }
MCWorkingCopyBrowser class >> repositorySearchMaxSize [
	^ repositorySearchMaxSize ifNil: [repositorySearchMaxSize := 15]

]

{ #category : #icons }
MCWorkingCopyBrowser class >> taskbarIconName [
	"Answer the icon for the receiver in a task bar."

	^#versionControlIcon
]

{ #category : #settings }
MCWorkingCopyBrowser class >> workingCopySearchList [
	^ workingCopySearchList ifNil: [ workingCopySearchList := OrderedCollection new ].
]

{ #category : #settings }
MCWorkingCopyBrowser class >> workingCopySearchMaxSize [
	^ workingCopySearchMaxSize ifNil: [workingCopySearchMaxSize := 15]

]

{ #category : #actions }
MCWorkingCopyBrowser >> addMetacelloConfiguration [
	|name|
	name := UIManager default 
				request: 'Name of the new Metacello configuration (e.g., ConfigurationOfYourSoftware)' translated 
				initialAnswer: 'ConfigurationOf'. 
	name isEmptyOrNil ifFalse:
		[ 
			"Check if the class does not exist"
			(Smalltalk globals includesKey: name asSymbol) ifTrue: [ self error: 'Class already exist'].

			"Check if metacello is loaded"
			(Smalltalk globals includesKey: #MetacelloConfigTemplate) ifFalse: 
				[Gofer new
  					squeaksource: 'MetacelloRepository';
  					package: 'ConfigurationOfMetacello';
 					 load.
				(Smalltalk at: #ConfigurationOfMetacello) perform: #loadLatestVersion].

			"Create the configuration"
			((Smalltalk globals at: #MetacelloConfigTemplate) duplicateClassWithNewName: name asSymbol)
        			category: name asString.

			"We create the package that has the same name"
			RPackageOrganizer default registerPackageNamed: name.
			
			"Select the package"
			workingCopy := MCWorkingCopy forPackage: (MCPackage new name: name).
			repository ifNotNil: [ workingCopy repositoryGroup addRepository: repository ].
			workingCopyWrapper := nil.
			workingCopy modified: true.
			self workingCopySelection: 0.
			self repositorySelection: 0 ].

	self workingCopyListChanged; 
		changed: #workingCopySelection;
		repositoryListChanged.
	self changedButtons.
]

{ #category : #actions }
MCWorkingCopyBrowser >> addRepository [
	self class newRepository ifNotNil: [:repos | 
		self addRepository: repos.
		workingCopy ifNil: [ repos morphicOpen: nil ]].

]

{ #category : #actions }
MCWorkingCopyBrowser >> addRepository: aRepository [
	self repository: aRepository.
	self repositoryGroup addRepository: aRepository.
	self updateRepositoryList.
]

{ #category : #actions }
MCWorkingCopyBrowser >> addRepositoryToPackage [

	self repository
		ifNotNil: [ :repos | 
			((SearchFacade mcPackageSearchRejectAll: [ :aPackage | aPackage repositoryGroup includes: repos ] withInitialFilter: nil)
				chooseFromOwner: self window)
				ifNotNil: [ :wc | 
					workingCopy := wc.
					workingCopy repositoryGroup addRepository: repos.
					self repository: repos.
					self
						changed: #workingCopySelection;
						repositoryListChanged;
						changed: #repositorySelection.
					self changedButtons ] ]
]

{ #category : #actions }
MCWorkingCopyBrowser >> addRepositoryToWorkingCopy [
	workingCopy ifNotNil:
		[:wc |
			workingCopy repositoryGroup addRepository: self repository.
			self
				changed: #workingCopySelection;
				repositoryListChanged;
				changed: #repositorySelection.
			self changedButtons]
]

{ #category : #actions }
MCWorkingCopyBrowser >> addRequiredDirtyPackage [
	| dirtyPackages |
	
	dirtyPackages := self dirtyPackages.
	
	workingCopy ifNotNil: [:wc |
		dirtyPackages do: [:required |
			wc = required
				ifFalse: [ wc requirePackage: required package]]].
	
	self workingCopyListChanged
]

{ #category : #actions }
MCWorkingCopyBrowser >> addRequiredPackage [
	| chosen |
	workingCopy
		ifNotNil: [ :wc | 
			chosen := (SearchFacade mcPackageSearchRejectAll: [ :ea | ea = wc or: [ wc requiredPackages includes: ea package ] ])
				chooseFromOwner: self window.
			chosen
				ifNotNil: [ 
					wc requirePackage: chosen package.
					self workingCopyListChanged ] ]
]

{ #category : #actions }
MCWorkingCopyBrowser >> addSlice [
	(MCSliceMaker openFor: self window)
		ifNotNil: [:sliceInfo | workingCopy := sliceInfo makeSlice.
			workingCopy ifNotNil: [
				workingCopy repositoryGroup 
					addRepository: MCHttpRepository pharoRepository;
					addRepository: MCHttpRepository pharoInboxRepository.
				workingCopyWrapper := MCDependentsWrapper with: workingCopy model: self.
					self repositorySelection: 0.]].
	self workingCopyListChanged; changed: #workingCopySelection; repositoryListChanged.
	self changedButtons.

]

{ #category : #actions }
MCWorkingCopyBrowser >> addWorkingCopy [
	| name |
	
	name := UIManager default request: 'Name of package:'.
	name isEmptyOrNil ifFalse: [
		RPackageOrganizer default registerPackageNamed: name.		
		workingCopy := MCWorkingCopy forPackage: (MCPackage new name: name).
		workingCopyWrapper := MCDependentsWrapper with: workingCopy model: self.
		self repositorySelection: 0 ].
	
	self 
		workingCopyListChanged; 
		changed: #workingCopySelection; 
		repositoryListChanged.
	self changedButtons.
]

{ #category : #actions }
MCWorkingCopyBrowser >> backportChanges [
	self canBackport ifFalse: [^self].
	workingCopy ifNotNil:
		[workingCopy needsSaving ifTrue: [^ self inform: 'You must save the working copy before backporting.'].
		self pickAncestorVersionInfo ifNotNil:
			[:baseVersionInfo |
			workingCopy backportChangesTo: baseVersionInfo]]
]

{ #category : #actions }
MCWorkingCopyBrowser >> basicSaveVersionIn: aRepository [
	| newVersion waitForVersion |
	waitForVersion := Semaphore new.
	
	UIManager default defer: [
		newVersion := workingCopy newVersionIn: aRepository.
		waitForVersion signal ].
	
	Processor activeProcess == UIManager default uiProcess
		ifFalse: [ waitForVersion wait ].
	newVersion ifNil: [ ^ self ]. 
	
	Cursor wait showWhile: [[ 
		self
			storeVersion: newVersion in: aRepository;
			storeDependencies: newVersion in: aRepository.] 
			ensure: [ (MCVersionInspector new version: newVersion) show ]]
]

{ #category : #actions }
MCWorkingCopyBrowser >> browseWorkingCopy [
	workingCopy ifNotNil:
		[(MCSnapshotBrowser forSnapshot: workingCopy completeSnapshot)
			label: 'Snapshot Browser: ', workingCopy packageName;
			show]
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> buildWindow [ 	

	|window|
	window := super buildWindow.
	self repositoryListChanged.
	^window
]

{ #category : #'menu spec' }
MCWorkingCopyBrowser >> buttonSpecs [
	^ #(#('+Package' #addWorkingCopy 'Add a new package and make it the working copy')
		#('+Config' #addMetacelloConfiguration 'Add a new metacello configuration')
		#('+Slice' #addSlice 'Add a slice to cleaning and nicely propose a bug fix')
		#('Browse' #browseWorkingCopy 'Browse the working copy of the selected package' #hasWorkingCopy)
		#('Changes' #viewChanges 'View the working copy''s changes relative to the installed version from the repository' #canSave)
	 	#('+Repository' #addRepository 'Add an existing repository to the list of those visible')
		#('Save' #saveVersion 'Save the working copy as a new version to the selected repository' #canSave)
		#('Open' #openRepository 'Open a browser on the selected repository' #hasRepository))
]

{ #category : #actions }
MCWorkingCopyBrowser >> canBackport [
	^ self hasWorkingCopy and: [workingCopy needsSaving not]
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> canSave [
	^ self hasWorkingCopy and: [self hasRepository]
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> changedButtons [
	self changed: #hasWorkingCopy.
	self changed: #canSave.
	self changed: #canBackport.
	self changed: #hasRepository.

]

{ #category : #actions }
MCWorkingCopyBrowser >> clearRequiredPackages [
	workingCopy ifNotNil:
		[:wc |
		wc clearRequiredPackages.
		self workingCopyListChanged]
]

{ #category : #'menu spec' }
MCWorkingCopyBrowser >> collectConfigurationMenuSpecs [
	| menuSpecs | 
	menuSpecs := (Pragma allNamed: #'contextMenu:' in: self class)
		select: [ :each | (each argumentAt: 1) = #configuration ].
		
	^ menuSpecs 
		inject: #()
		into: [ :all :each | all, (each method valueWithReceiver: self arguments: #()) ]
	
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> currentShowOnlyRepositoriesFromWorkingCopy [
	^showOnlyRepositoriesFromWorkingCopy ifTrue: [ 2] 
	ifFalse: [ 1] 
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> defaultLabel [
	^ 'Monticello Browser'
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> defaults [
	^ defaults ifNil: [defaults := Dictionary new]
]

{ #category : #actions }
MCWorkingCopyBrowser >> deleteWorkingCopy [
	workingCopy unregister.
	self workingCopySelection: 0.
	self workingCopyListChanged.
]

{ #category : #actions }
MCWorkingCopyBrowser >> dirtyPackages [
	^ self workingCopies select: [:copy | copy needsSaving].

]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> editLoadScripts [

	| menu |
	self hasWorkingCopy ifFalse: [^self].
	menu := UIManager default newMenuIn: self for: self.
	menu add: 'edit preamble' selector: #editScript: argument: #preamble.
	menu add: 'edit postscript' selector: #editScript: argument: #postscript.
	menu add: 'edit preambleOfRemoval' selector: #editScript: argument: #preambleOfRemoval.
	menu add: 'edit postscriptOfRemoval' selector: #editScript: argument: #postscriptOfRemoval.
	menu popUpInWorld.
]

{ #category : #actions }
MCWorkingCopyBrowser >> editRepository [
	| newRepo |
	newRepo := self repository openAndEditTemplateCopy.
	newRepo ifNotNil: [ 
		(newRepo isKindOf: MCRepository)
			ifFalse: [ 
				self inform: 'Got a ', newRepo class name, ' instead of a Monticello Repository'].
		self flag: 'Is this ugly? yes!'.
		self repository becomeForward: newRepo ].
	self repositoryListChanged
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> editScript: scriptSymbol [

	| script |
	script := workingCopy packageSet perform: scriptSymbol.
	script openLabel: scriptSymbol asString, ' of the Package ', workingCopy package name.
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> enterIssueNumber [
	| issue |
	issue := UIManager default request: 'Please give the issue number' initialAnswer: '00000'.
	issue isEmptyOrNil ifFalse: [
		issue isAllDigits ifFalse: [
			UIManager default inform: 'You must give us a number'.
			issue := self enterIssueNumber]].
	^ issue
	
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> enterSliceName [
	| name |
	name := UIManager default request: 'Please describe the fix' initialAnswer: 'OneSentenceSummary'.
		name isEmptyOrNil ifFalse: [
		name isAllAlphaNumerics ifFalse:[
			UIManager default inform:'Your sentence can only be composed of alphanumerics caracters'.
			name := self enterSliceName]].
	^ name
]

{ #category : #actions }
MCWorkingCopyBrowser >> flushAllCaches [
	| beforeBytes afterBytes beforeVersions afterVersions |
	Cursor wait showWhile: [
		beforeBytes := Smalltalk garbageCollect.
		beforeVersions := MCVersion allSubInstances size.
		MCFileBasedRepository flushAllCaches.
		afterBytes := Smalltalk garbageCollect.
		afterVersions := MCVersion allSubInstances size.
	].
	^self inform: (beforeVersions - afterVersions) asString, ' versions flushed', String cr,
 		(afterBytes - beforeBytes) asStringWithCommas, ' bytes reclaimed'
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> getShowOnlyRepositoriesFromWorkingCopySettings [
	^#('All' 'Package')
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> hasRepository [
	^ self repository notNil
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> hasWorkingCopy [
	^ workingCopy notNil
]

{ #category : #'menu spec' }
MCWorkingCopyBrowser >> historyMenuSpec [

	^ #(#('Browse package' #browseWorkingCopy)
		#('View changes' #viewChanges) 
		#('View history' #viewHistory))

]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> initialExtent [
	^ 650.0 @ 420.0
]

{ #category : #initialization }
MCWorkingCopyBrowser >> initialize [
	super initialize.
	order := self class order.
	showOnlyRepositoriesFromWorkingCopy := true.
	self registerToAnnouncer.
	workingCopyPattern := ''.
	repositoryPattern := ''
]

{ #category : #actions }
MCWorkingCopyBrowser >> inspectWorkingCopy [
	workingCopy ifNotNil: [workingCopy inspect]
]

{ #category : #actions }
MCWorkingCopyBrowser >> loadRepositories [
	FileStream fileIn: 'MCRepositories.st'.
	self repositoryListChanged.
	self changedButtons.

]

{ #category : #actions }
MCWorkingCopyBrowser >> openRepository [
	self repository ifNotNil: [:repos | 
		repos morphicOpen: workingCopy ]
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> order: anInteger [
	"
	Set the order of the listed working copies
	anInteger represents the order of the mcworkingcopies in the MCBrowser. Its value is between 0 and the number of entries in MCFileRepositoryInspector >> orderSpecs
	"
	
	self class order: (order := anInteger).
	self changed: #workingCopyList; changed: #workingCopyTree
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> orderSpecs [
	^ {
		'Sort alphabetically' -> [ :a :b | a package name <= b package name ].
		'Sort dirty first' -> [ :a :b | 
			a needsSaving = b needsSaving
				ifTrue: [ a package name <= b package name ]
				ifFalse: [ a needsSaving ] ].
		'Sort dirty last' -> [ :a :b | 
			a needsSaving = b needsSaving
				ifTrue: [ a package name <= b package name ]
				ifFalse: [ b needsSaving ] ].
		'Only dirty' -> [ :a :b | a package name <= b package name ]
	}
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> orderString: anIndex [
	^ String streamContents: [ :stream |
		order = anIndex
			ifTrue: [ stream nextPutAll: '<yes>' ]
			ifFalse: [ stream nextPutAll: '<no>' ].
		stream nextPutAll: (self orderSpecs at: anIndex) key ]
]

{ #category : #'menu spec' }
MCWorkingCopyBrowser >> packageMenuSpec [

	^ #(#('Add required package' #addRequiredPackage)
		#('Add all dirty packages as required' #addRequiredDirtyPackage)
		#('Remove required package' #removeRequiredPackage)
		#('Clear required packages' #clearRequiredPackages)
		#('View past comments' #viewPastComments))

]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> pickAncestorVersionInfo [
	| ancestors index |
	ancestors := workingCopy ancestry breadthFirstAncestors.
	index := UIManager default chooseFrom: (ancestors collect: [:ea | ea name])
				title: 'Ancestor:'.
	^ index = 0 ifFalse: [ ancestors at: index]
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> pickWorkingCopy [
	^self pickWorkingCopySatisfying: [ :c | true ]
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> pickWorkingCopySatisfying: aBlock [
	| copies index |
	copies := self workingCopies select: aBlock.
	copies isEmpty ifTrue: [ ^nil ].
	index := UIManager default chooseFrom: (copies collect: [:ea | ea packageName])
				title: 'Package:'.
	^ index = 0 ifFalse: [ copies at: index]
]

{ #category : #actions }
MCWorkingCopyBrowser >> recompilePackage [
	workingCopy package packageSet methods
		do: [:ea | ea actualClass recompile: ea selector]
		displayingProgress: 'Recompiling...'
]

{ #category : #initialization }
MCWorkingCopyBrowser >> registerToAnnouncer [
	SystemAnnouncer uniqueInstance weak
		when: MCPackageModified send: #update: to: self;
		when: MCWorkingCopyCreated send: #update: to: self;
		when: MCWorkingCopyDeleted send: #update: to: self;
		when: MCRepositoryAdded send: #updateRepositoryList to: self;
		when: MCRepositoryRemoved send: #updateRepositoryList to: self.
]

{ #category : #actions }
MCWorkingCopyBrowser >> removeRepository [
	self repository ifNotNil:
		[:repos |
		self repositoryGroup removeRepository: repos.
		self repositorySelection: (1 min: self repositories size)].
	self repositoryListChanged.
	self changedButtons.

]

{ #category : #actions }
MCWorkingCopyBrowser >> removeRequiredPackage [

	| allRequiredPackages |
	workingCopy ifNil: [^ self].
	allRequiredPackages := workingCopy requiredPackages.
	allRequiredPackages ifEmpty: [UIManager default inform: 'This package has no requirements'. ^ self].
	(self pickWorkingCopySatisfying: [:wc | allRequiredPackages includes: wc package]) 
		ifNotNil:
			[:required |
			workingCopy removeRequiredPackage: required package.
			self workingCopyListChanged]
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> repositories [
	"Return the list of MCRepository, typically SmalltalkHubRepository. This methods drives what is displayed on the list on the right hand side of the working copy browser.
	It contains at least one cache repository, corresponding to the local package cache"
	| repositories |
	repositories := self repositoryGroup repositories.
	
	"filter the repository list if there is a search string"
	repositoryPattern ifNotEmpty: [
		repositories := repositories select: [ :each| each description asLowercase includesSubstring: repositoryPattern ]].
	^ repositories
]

{ #category : #actions }
MCWorkingCopyBrowser >> repository [
	^ repository
]

{ #category : #actions }
MCWorkingCopyBrowser >> repository: aRepository [
	repository := aRepository.
	workingCopy ifNotNil: [self defaults at: workingCopy put: aRepository]
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> repositoryGroup [
	"Return the group of repository displayed in the monticello browser"
	(showOnlyRepositoriesFromWorkingCopy and: [ workingCopy isNotNil ])
		ifTrue: [ ^ workingCopy repositoryGroup ].
	^ MCRepositoryGroup default
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> repositoryIcon: a [
	^ self iconNamed: #monticelloPackageIcon
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> repositoryList [
	"Return the list of repository names. Only String are returned"
	| repositories |
	repositories := self repositories.

	^ repositories collect: [:ea | ea description]
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> repositoryListChanged [
	self changed: #repositoryList.
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> repositoryListMenu: aMenu [
	self repository ifNil: [^ aMenu].
	self fillMenu: aMenu fromSpecs: self repositoryMenuSpec.
	^ aMenu
				
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> repositoryMaxSearchSize [
	^self class repositoryMaxSearchSize
]

{ #category : #'menu spec' }
MCWorkingCopyBrowser >> repositoryMenuSpec [
	^ 	#(('Open repository' #openRepository)
		   ('Edit repository info' #editRepository)
		   ('Add to package...' #addRepositoryToPackage)
		   ('Remove repository' #removeRepository)	
		   ('Load repositories' #loadRepositories)	
		   ('Save repositories' #saveRepositories)
		   ('Flush cached versions' #flushAllCaches))
		
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> repositorySearchAccept: string [
	| aString |

	aString := string ifNil: [ '' ].
	repositoryPattern = aString asLowercase ifTrue: [ ^ self ].
	repositoryPattern := aString asLowercase.
		
	repositoryProcess ifNotNil: [ repositoryProcess terminate ].
	repositoryProcess := [ self repositoryListChanged ] fork.
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> repositorySearchField [
	^ SearchMorph new
		model: self;
		setIndexSelector: #repositorySearchAccept:; 
		updateSelector: #repositorySearchAccept:;
		searchList: self class repositorySearchList;
		yourself
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> repositorySearchMaxSize [
	^self class repositorySearchMaxSize
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> repositorySelection [
	"Return the index of the selected repository.
	May return 0, which means that no repository is selected"
	^ self repositories indexOf: self repository
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> repositorySelection: aNumber [
	aNumber = 0
		ifTrue: [self repository: nil]
		ifFalse: [self repository: (self repositories at: aNumber)].
	self changed: #repositorySelection.
	self changedButtons.

]

{ #category : #actions }
MCWorkingCopyBrowser >> revertPackage [
	self pickAncestorVersionInfo ifNotNil: [:info |
		(self repositoryGroup versionWithInfo: info
			ifNone: [^self inform: 'No repository found for ', info name]
		) load]
]

{ #category : #'menu spec' }
MCWorkingCopyBrowser >> revertPackageMenuSpec [

	^ #(#('Revert package...' #revertPackage) 
			   #('Recompile package' #recompilePackage)
			   #('Backport package...' #backportChanges)
			   #('Inspect package' #inspectWorkingCopy))

]

{ #category : #actions }
MCWorkingCopyBrowser >> saveRepositories [

	FileStream forceNewFileNamed: 'MCRepositories.st' do: [:f |
		MCRepositoryGroup default repositoriesDo: [:r |
			r asCreationTemplate ifNotNil: [:template |
				f nextPutAll: 'MCRepositoryGroup default addRepository: (', template , ')!'; cr]]]
]

{ #category : #actions }
MCWorkingCopyBrowser >> saveVersion [
	self canSave ifFalse: [ ^ self ].
	"fork the whole version creation "
	[ self basicSaveVersionIn: self repository ] fork
]

{ #category : #actions }
MCWorkingCopyBrowser >> setShowOnlyRepositoriesFromWorkingCopy: anIndex [
	showOnlyRepositoriesFromWorkingCopy  := (anIndex = 2).
	self repositorySelection: 0.
	self repositoryListChanged.

]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> showOnlyRepositoriesFromWorkingCopyField [
	^(DropListMorph
		on: self 
		list: #getShowOnlyRepositoriesFromWorkingCopySettings
		selected: #currentShowOnlyRepositoriesFromWorkingCopy
		changeSelected: #setShowOnlyRepositoriesFromWorkingCopy:)
		listPaneColor: Color transparent;
		yourself			

]

{ #category : #actions }
MCWorkingCopyBrowser >> storeDependencies: newVersion in: aRepository [
	aRepository storeDependencies: newVersion 
]

{ #category : #actions }
MCWorkingCopyBrowser >> storeVersion: newVersion in: aRepository [
	aRepository storeVersion: newVersion
]

{ #category : #actions }
MCWorkingCopyBrowser >> unloadPackage [
	workingCopy unload.
	self workingCopySelection: 0.
	self workingCopyListChanged.
]

{ #category : #'menu spec' }
MCWorkingCopyBrowser >> unloadPackageMenuSpec [

	^ #(#('Unload package' #unloadPackage)
		#('Delete working copy' #deleteWorkingCopy))
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> unsortedWorkingCopies [
	^ self allManagers 
]

{ #category : #updating }
MCWorkingCopyBrowser >> update: anAnnouncement [
	self workingCopyListChanged.
]

{ #category : #updating }
MCWorkingCopyBrowser >> updateRepositoryList [
	self repositoryListChanged.
	self changed: #repositorySelection.
	self changedButtons.
]

{ #category : #actions }
MCWorkingCopyBrowser >> viewHistory [
	workingCopy ifNotNil:
		[(MCWorkingHistoryBrowser new
				ancestry: workingCopy ancestry;
				package: workingCopy package)
			label:  'Version History: ',  workingCopy packageName;
			show]
]

{ #category : #actions }
MCWorkingCopyBrowser >> viewPastComments [
	"Visualize past comments on a package."

       workingCopy ifNotNil:
               [(UIManager default
                       edit: (String streamContents:
                                       [:s|
                                       "Ignore the initial MCWorkingAncestry instance."
                                       workingCopy ancestry topologicalAncestors allButFirst
                                               do: [:versionInfo|
                                                       s nextPutAll: versionInfo summary]
                                               separatedBy: [s cr; cr"; next: 32 put: $-; cr; cr"]])
                       label: 'Version History: ',  workingCopy packageName) ]
]

{ #category : #'menu spec' }
MCWorkingCopyBrowser >> widgetSpecs [
	| searchBarOffset |
	
	searchBarOffset := 30 + StandardFonts defaultFont height + 10.
	
	^ {
		"send fractions offsets"
		{{#buttonRow}. {0. 0. 1. 0}. {0. 0. 0. 30.}}.
		
		{{#workingCopySearchField}. {0. 0. 0.5. 0.}. {0. 30. 0. searchBarOffset.}}.
		{{#treeMorph:. #workingCopy}. {0. 0. 0.5. 1}. {0. searchBarOffset+3. 0. 0.}}.
		
		{{#repositorySearchField}. {0.5. 0. 0.85. 0.}. {0. 30. 0. searchBarOffset.}}.
		{{#showOnlyRepositoriesFromWorkingCopyField}. { 0.85. 0. 1. 0.}. { 0. 30. 0. searchBarOffset .}}.
		{{#listMorph:selection:menu:icon: . #repositoryList . #repositorySelection . #repositoryListMenu: . #repositoryIcon:}. {0.5. 0. 1. 1}. {0. searchBarOffset+3. 0. 0.}}.
		}.
]

{ #category : #updating }
MCWorkingCopyBrowser >> windowIsClosing [
	
	SystemAnnouncer uniqueInstance unsubscribe: self.
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> workingCopies [
	^ (self orderSpecs size = order
		ifTrue: [ self allManagers select: [ :each | each modified ] ]
		ifFalse: [ self allManagers ])
			asSortedCollection: (self orderSpecs at: order) value.
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> workingCopy: wc [
	workingCopy := wc.
	self changed: #workingCopySelection; repositoryListChanged.
	self changedButtons.

]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> workingCopyList [
	^ self workingCopies collect:
		[:ea |
		(workingCopy notNil and: [workingCopy requiredPackages includes: ea package])
			ifTrue: [Text string: ea description]
			ifFalse: [ea description]]
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> workingCopyListChanged [
	self changed: #workingCopyList.
	self changed: #workingCopyTree.
	self changed: #workingCopyTreePath.
	self changedButtons.

]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> workingCopyListMenu: aMenu [

	workingCopy ifNil: [ ^ aMenu ].
	(workingCopy package name beginsWith: 'ConfigurationOf') ifTrue: [
		self fillMenu: aMenu fromSpecs: self collectConfigurationMenuSpecs.
		aMenu addLine ].
	self fillMenu: aMenu fromSpecs: self historyMenuSpec.
	aMenu addLine.
	self fillMenu: aMenu fromSpecs: self packageMenuSpec.
	aMenu addLine.
	self fillMenu: aMenu fromSpecs: self revertPackageMenuSpec.
	aMenu addLine.
	self fillMenu: aMenu fromSpecs: self unloadPackageMenuSpec.
	aMenu addLine.
	1 to: self orderSpecs size do: [ :index | 
		aMenu
			addUpdating: #orderString:
			target: self
			selector: #order:
			argumentList: {index} ].
	^ aMenu 
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> workingCopySearchAccept: string [
	| aString |

	aString := string ifNil: [ '' ].
	workingCopyPattern = aString asLowercase ifTrue: [ ^ self ].
	workingCopyPattern := aString asLowercase.
		
	packageProcess ifNotNil: [ packageProcess terminate ].	
	packageProcess := [ self workingCopyListChanged ] fork.
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> workingCopySearchAdd: aString [ 
	self workingCopySearchList: (self workingCopySearchList
			remove: aString
			ifAbsent: [];
			 yourself).
	self workingCopySearchList size = self workingCopySearchMaxSize
		ifTrue: [self workingCopySearchList removeLast].
	self workingCopySearchList addFirst: aString 
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> workingCopySearchField [
	^ SearchMorph new
		model: self;
		setIndexSelector: #workingCopySearchAccept:; 
		searchList: self class workingCopySearchList;
		updateSelector: #workingCopySearchAccept:;
		yourself

]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> workingCopySearchMaxSize [
	^self class workingCopySearchMaxSize
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> workingCopySelection [
	^ self workingCopies indexOf: workingCopy
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> workingCopySelection: aNumber [
	self workingCopy: 
		(aNumber = 0 
			ifTrue: [nil]
			ifFalse: [self workingCopies at: aNumber]).	
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> workingCopySelectionWrapper [
	^workingCopyWrapper
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> workingCopySelectionWrapper: aWrapper [
	workingCopyWrapper := aWrapper.
	self changed: #workingCopySelectionWrapper.
	self workingCopy: (aWrapper ifNotNil:[aWrapper item])
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> workingCopyTree [
	| workingCopies |
	workingCopies := self workingCopies.
	
	"filter the working copy list if there is a serach string"
	workingCopyPattern ifNotEmpty: [ 
		workingCopies := workingCopies select: [ :each| each package name asLowercase includesSubstring: workingCopyPattern ]].
			
	^ workingCopies collect: [:each| MCDependentsWrapper with: each model: self].
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> workingCopyTreeChildren: aWrapper [
	^ aWrapper contents
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> workingCopyTreeHasChildren: aWrapper [
	^ aWrapper hasContents
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> workingCopyTreeLabel: aWrapper [
	^ aWrapper asString
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> workingCopyTreeMenu: aMenu [
	^ self workingCopyListMenu: aMenu
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> workingCopyTreeMorph [
	^ SimpleHierarchicalListMorph
		on: self
		list: #workingCopyTree
		selected: #workingCopyWrapper
		changeSelected: #workingCopyWrapper:
		menu: #workingCopyListMenu:
]

{ #category : #'morphic ui' }
MCWorkingCopyBrowser >> workingCopyTreePath [
	workingCopy ifNil: [^ OrderedCollection new].
	^ OrderedCollection with: workingCopyWrapper.
]

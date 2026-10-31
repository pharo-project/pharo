"
Unit tests for QuickProjectLoaderPresenter main UI.
"
Class {
	#name : 'QuickProjectLoaderPresenterTest',
	#superclass : 'TestCase',
	#instVars : [
		'presenter',
		'repository',
		'tempFile'
	],
	#category : 'QuickProjectLoader-Tests',
	#package : 'QuickProjectLoader-Tests'
}

{ #category : 'running' }
QuickProjectLoaderPresenterTest >> setUp [

	super setUp.
	tempFile := FileSystem memory / 'test_presenter_repo.ston'.
	repository := QuickProjectRepository new.
	repository persistencePath: tempFile.

	(repository addEntryNamed: 'Roassal' repository: 'github://ObjectProfile/Roassal/src' baseline: 'Roassal') isManual: true.
	(repository addEntryNamed: 'Seaside' repository: 'github://SeasideSt/Seaside:v3.4.6/repository' baseline: 'Seaside') isManual: true.

	presenter := QuickProjectLoaderPresenter new.
	presenter repository: repository
]

{ #category : 'running' }
QuickProjectLoaderPresenterTest >> tearDown [

	QuickProjectLoaderPresenter closeWindow.
	QuickProjectRepository reset.
	tempFile deleteIfAbsent: [].
	super tearDown
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testFilterProjects [

	presenter filterProjects: 'Seaside'.
	self assert: presenter items size equals: 1.
	self assert: presenter items first name equals: 'Seaside'.

	presenter filterProjects: ''.
	self assert: presenter items size equals: 2
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testKeymapRegistration [

	self assert: (KMRepository default globalCategories anySatisfy: [ :each | each name = #QuickProjectLoaderCategory ])
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testPresenterInitializes [

	self assert: presenter items size equals: 2
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testPresenterShowsEntries [

	| items |
	items := presenter items.
	self assert: items size equals: 2.
	self assert: (items anySatisfy: [ :e | e name = 'Roassal' ]).
	self assert: (items anySatisfy: [ :e | e name = 'Seaside' ])
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testPresenterSortsEntries [

	| seaside |
	seaside := repository rawEntries detect: [ :e | e name = 'Seaside' ].
	repository recordLoadOf: seaside.
	presenter refreshList.

	self assert: presenter items first name equals: 'Seaside'
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testWorldMenuRegistration [

	self assert: ((QuickProjectLoaderPresenter class >> #menuCommandOn:) hasPragmaNamed: #worldMenu)
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testActionLabels [

	self assert: (presenter actionLabelFor: (QuickProjectEntry name: 'SurelyNotInThisImage' repositoryUrl: 'github://a/b')) equals: 'Load'.
	self
		assert: (presenter actionLabelFor: (QuickProjectEntry name: 'Metacello' repositoryUrl: 'github://a/b'))
		equals: 'Reload'
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testLoadRunsInTheBackgroundOnlyOnce [

	| loaded release entry |
	loaded := OrderedCollection new.
	release := Semaphore new.
	entry := QuickProjectLoadRecorder new
		name: 'Stub';
		onLoad: [
			loaded add: Processor activeProcess.
			release wait ];
		yourself.

	presenter loadProject: entry.
	presenter loadProject: entry.
	100 milliSeconds wait.

	self assert: loaded size equals: 1.
	self deny: loaded first identicalTo: Processor activeProcess.
	release signal.
	100 milliSeconds wait
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testOfferIsSkippedWithoutProjects [

	| previous |
	previous := QuickProjectRepository default.
	QuickProjectRepository default: (QuickProjectRepository new
			persistencePath: FileSystem memory / 'empty.ston';
			yourself).
	[ self assert: QuickProjectLoaderPresenter offer identicalTo: QuickProjectLoaderPresenter ]
		ensure: [
			QuickProjectRepository default: previous.
			QuickProjectLoaderPresenter resetOffered ]
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testOfferIsOnlyMadeOnce [

	| previous |
	previous := QuickProjectRepository default.
	QuickProjectRepository default: repository.
	[
		QuickProjectLoaderPresenter offer.
		self deny: QuickProjectLoaderPresenter shouldOfferOnStartup ] ensure: [
		QuickProjectRepository default: previous.
		QuickProjectLoaderPresenter resetOffered ]
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testPanelHeightFollowsItsRows [

	| window |
	window := QuickProjectLoaderPresenter.
	self assert: (window extentForRows: 5) y > (window extentForRows: 3) y.
	self assert: (window extentForRows: 0) equals: (window extentForRows: 1).
	self assert: (window extentForRows: 50) equals: (window extentForRows: 7).
	self assert: (window extentForRows: 7) y > (window extentForRows: 6) y
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testRemoveProject [

	presenter removeProject: presenter items first.

	self assert: presenter items size equals: 1
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testRowActionOnlyFiresInsideTheAction [

	| row loaded menus |
	loaded := 0.
	menus := 0.
	row := QuickProjectRowMorph
		entry: (QuickProjectEntry name: 'A' repositoryUrl: 'github://a/b')
		action: 'Load'
		onLoad: [ loaded := loaded + 1 ]
		onMenu: [ menus := menus + 1 ].
	row bounds: (0 @ 0 extent: 288 @ 46).

	row mouseDown: (MouseButtonEvent new setType: #mouseDown position: 5 @ 5 which: 4 buttons: 4 hand: nil stamp: 0).
	self assert: loaded equals: 0.

	row mouseDown: (MouseButtonEvent new setType: #mouseDown position: row actionBounds center which: 4 buttons: 4 hand: nil stamp: 0).
	self assert: loaded equals: 1.

	row mouseDown: (MouseButtonEvent new setType: #mouseDown position: 5 @ 5 which: 2 buttons: 2 hand: nil stamp: 0).
	self assert: menus equals: 1
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testLoadBranchSetsTheBranchAndLoads [

	| entry loaded |
	loaded := false.
	entry := QuickProjectLoadRecorder new
		name: 'Stub';
		onLoad: [ loaded := true ];
		yourself.

	presenter loadBranch: 'dev' of: entry.
	100 milliSeconds wait.

	self assert: loaded.
	self assert: entry branch equals: 'dev'
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testBranchButtonSitsBesideTheAction [

	| row |
	row := presenter rowFor: presenter items first.
	row bounds: (0 @ 0 extent: 288 @ 46).

	self assert: row branchButton right < row actionBounds left.
	self assert: row branchButton left > row left.
	self assert: row branchButton center y equals: row center y
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testBranchesLineUpAcrossRows [

	| rows |
	rows := #( 'Load' 'Reload' ) collect: [ :label |
		(QuickProjectRowMorph
			entry: presenter items first
			action: label
			onLoad: [  ]
			onMenu: [  ])
			branchBlock: [  ];
			bounds: (0 @ 0 extent: 340 @ 46);
			yourself ].

	self assert: rows first branchButton left equals: rows second branchButton left
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testBranchChipShowsTheBranchToLoad [

	| entry row |
	entry := QuickProjectEntry name: 'A' repositoryUrl: 'github://a/b:dev'.

	row := presenter rowFor: entry.

	self assert: row branchButton label equals: 'dev'.
	self assert: (presenter rowFor: presenter items first) branchButton label equals: 'default'
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testBranchChipCutsLongNames [

	| chip font fitted |
	chip := QuickProjectBranchChipMorph label: 'feature/a-very-long-branch-name' action: [  ].
	font := StandardFonts defaultFont.

	fitted := QuickProjectRowMorph fit: chip label font: font width: 60.

	self assert: (fitted endsWith: '...').
	self assert: (font widthOfString: fitted) <= 60
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testNoBranchButtonWhileLoading [

	| row |
	row := QuickProjectRowMorph
		entry: presenter items first
		action: 'Loading...'
		onLoad: [  ]
		onMenu: [  ].
	row branchBlock: [  ].

	self assert: row branchButton isNil
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testBranchMenuListsBranchesAndOther [

	| entry labels |
	entry := QuickProjectEntry name: 'A' repositoryUrl: 'github://a/b:dev'.

	labels := ((presenter branchMenuFor: entry names: #( 'main' 'dev' )) items
		collect: [ :each | each contents asString ]).

	self assert: (labels includesAll: #( 'main' 'dev' 'Other...' ))
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testBranchMenuIsCapped [

	| names labels |
	names := (1 to: 50) collect: [ :i | 'branch' , i printString ].

	labels := ((presenter branchMenuFor: presenter items first names: names) items
		collect: [ :each | each contents asString ]).

	self assert: (labels select: [ :each | each beginsWith: 'branch' ]) size equals: QuickProjectLoaderPresenter maxBranchesInMenu
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testLongTextGetsAnEllipsis [

	| row font fitted |
	row := QuickProjectRowMorph new.
	font := StandardFonts defaultFont.

	fitted := row fit: 'a-really-long-organization/a-really-long-repository' font: font width: 80.

	self assert: (fitted endsWith: '...').
	self assert: (font widthOfString: fitted) <= 80.
	self assert: (row fit: 'short' font: font width: 80) equals: 'short'
]

{ #category : 'tests' }
QuickProjectLoaderPresenterTest >> testIconsAreTintedWithTheTheme [

	| source chip |
	source := Form extent: 2 @ 2 depth: 32.
	source fillColor: Color red.

	chip := QuickProjectIconButtonMorph icon: source help: '' action: [  ].

	self deny: ((chip instVarNamed: #icon) colorAt: 0 @ 0) = Color red.
	self
		assert: ((chip instVarNamed: #icon) colorAt: 0 @ 0) rgbTriplet
		equals: chip tintColor rgbTriplet
]

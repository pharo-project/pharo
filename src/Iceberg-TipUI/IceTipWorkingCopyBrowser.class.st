"
I'm a tool to show the internals of a repository from the point of view of an image. 
I show the packages managed by this repository and a series of actions to properly handle them in the context of an iceberg repository.

(IceTipPackagesBrowser forRepository: IceRepository registry anyOne) openWithSpec
"
Class {
	#name : #IceTipWorkingCopyBrowser,
	#superclass : #IceTipBrowser,
	#instVars : [
		'packageList',
		'statusBar'
	],
	#category : #'Iceberg-TipUI-View'
}

{ #category : #specs }
IceTipWorkingCopyBrowser class >> defaultSpec [
	<spec: #default>
	
	^ SpecLayout composed 
		newColumn: [ :column |
			column 
				newRow: #toolbar height: IceTipToolbar defaultHeight;
				newRow: #packageList;
				newRow: #statusBar height: self defaultStatusBarHeight ]
]

{ #category : #specs }
IceTipWorkingCopyBrowser class >> defaultStatusBarHeight [
	^ 25 * World displayScaleFactor
]

{ #category : #'instance creation' }
IceTipWorkingCopyBrowser class >> new [
	self error: 'Use #for:'
]

{ #category : #'instance creation' }
IceTipWorkingCopyBrowser class >> onRepository: aRepository [
	^ self on: (IceTipRepositoryModel on: aRepository)
]

{ #category : #accessing }
IceTipWorkingCopyBrowser >> contextClass [
	^ IceTipWorkingCopyContext
]

{ #category : #'private factory' }
IceTipWorkingCopyBrowser >> createCommandContext [
	^ self newContextWithSelection: self packageSelected
]

{ #category : #accessing }
IceTipWorkingCopyBrowser >> initialExtent [
	^ (700@500) scaledByDisplayScaleFactor
]

{ #category : #initialization }
IceTipWorkingCopyBrowser >> initializePackageList [
	packageList widget 
		beResizable;
		addColumn: (IceTipTableColumn new 
			id: 'Name';
			action: #descriptionWithDecoration;
			yourself);
		addColumn: (IceTipTableColumn new 
			id: 'Status';
			action: #statusWithDecoration;
			yourself);		
		dataSource: self newPackagesDataSource;
		selectRowIndex: 1;
		enableFilter: IceTipPackageFilter;			
		explicitFunction
]

{ #category : #initialization }
IceTipWorkingCopyBrowser >> initializeStatusBar [
	statusBar 
		addItem: (IceTipBranchWithCommitStatusBarItem new
			model: self model;
			yourself);
		addItem: (IceTipStatusBarTargetItem new 
			bePositionRight;
			target: self model;
			contents: #status;
			yourself)
]

{ #category : #initialization }
IceTipWorkingCopyBrowser >> initializeWidgets [
	super initializeWidgets.
	packageList := self newIceTable.
	statusBar := self instantiate: IceTipStatusBar.

	self initializePackageList.
	self initializeStatusBar.
	
	self focusOrder add: packageList
]

{ #category : #'private factory' }
IceTipWorkingCopyBrowser >> newContextWithSelection: anObject [ 
	^ self newContext 
		packageModel: anObject;
		yourself
]

{ #category : #'private factory' }
IceTipWorkingCopyBrowser >> newPackagesDataSource [
	^ IceTipDataSource new
		tool: self;
		elements: self model packageModels;
		yourself
]

{ #category : #'accessing ui' }
IceTipWorkingCopyBrowser >> packageList [
	^ packageList
]

{ #category : #'accessing ui' }
IceTipWorkingCopyBrowser >> packageList: anObject [
	packageList := anObject
]

{ #category : #private }
IceTipWorkingCopyBrowser >> packageSelected [
	| index dataSource |
	self flag: #todo. "This should -maybe- be modeled better on a SpecTablePresenter ;)"
	index := packageList widget selectedRowIndex.
	index = 0 ifTrue: [ ^ nil ].
	dataSource := packageList widget dataSource.
	dataSource hasElements ifFalse: [ ^ nil ].
	^ dataSource elementAt: index
]

{ #category : #accessing }
IceTipWorkingCopyBrowser >> refresh [
	self rebuildToolbar. 
	packageList widget 
		in: [ :this | this dataSource elements: self model packageModels ];
		refresh.
	statusBar refresh
]

{ #category : #private }
IceTipWorkingCopyBrowser >> refreshWhenRepository: ann [
	(ann repository isNil 
		or: [ self model repositoryModel isModelOf: ann repository ])
		ifFalse: [ ^ self ].
	self model reset.
	self refresh
]

{ #category : #initialization }
IceTipWorkingCopyBrowser >> setModelBeforeInitialization: aRepositoryModel [
	self model: (IceTipWorkingCopyModel on: aRepositoryModel)
]

{ #category : #'accessing ui' }
IceTipWorkingCopyBrowser >> statusBar [
	^ statusBar
]

{ #category : #'accessing ui' }
IceTipWorkingCopyBrowser >> statusBar: anObject [
	statusBar := anObject
]

{ #category : #initialization }
IceTipWorkingCopyBrowser >> subscribeToAnnouncements [
	Iceberg announcer weak 
		when: IceRepositoryAnnouncement 
		send: #refreshWhenRepository: 
		to: self

]

{ #category : #accessing }
IceTipWorkingCopyBrowser >> title [
	^ 'Working copy of ', self model repositoryName
]

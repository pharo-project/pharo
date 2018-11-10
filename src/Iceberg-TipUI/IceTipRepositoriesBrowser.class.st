"
I'm a tool to show all repositories registered on this image. 
I define basic actions and an entry point to manage each repository individually.

IceTipRepositoriesBrowser new openWithSpec.
"
Class {
	#name : #IceTipRepositoriesBrowser,
	#superclass : #IceTipBrowser,
	#instVars : [
		'repositoryList'
	],
	#category : #'Iceberg-TipUI-View'
}

{ #category : #specs }
IceTipRepositoriesBrowser class >> defaultSpec [
	<spec: #default>
	
	^ SpecLayout composed 
		newColumn: [ :column |
			column 
				newRow: #toolbar height: IceTipToolbar defaultHeight;
				newRow: #repositoryList ]
]

{ #category : #accessing }
IceTipRepositoriesBrowser class >> icon [
	^ self iconNamed: #komitterSmalltalkhubRemote
]

{ #category : #'world menu' }
IceTipRepositoriesBrowser class >> menuCommandOn: aBuilder [ 
	"Add a custom menu item to the world menu"	
	<worldMenu> 
	
	(aBuilder item: #'Iceberg')
		order: 0.8; 
		icon: self icon;  
		parent: #'Tools';
		keyText: 'o, i';
		help: 'Iceberg is a set of tools that allow one to handle git repositories directly from a Pharo image.';
		action: [ self new openWithSpec ]
]

{ #category : #accessing }
IceTipRepositoriesBrowser >> contextClass [
	^ IceTipRepositoryListContext
]

{ #category : #'private factory' }
IceTipRepositoriesBrowser >> createCommandContext [
	^ self newContextWithSelection: self repositorySelected
]

{ #category : #initialization }
IceTipRepositoriesBrowser >> initialize [
	self model: IceTipRepositoriesModel new.
	super initialize
]

{ #category : #initialization }
IceTipRepositoriesBrowser >> initializeRepositoryList [
	repositoryList widget 
		beResizable;
		addColumn: (IceTipTableColumn new 
			id: 'Name';
			action: #descriptionWithDecoration;
			width: 200;
			yourself);
		addColumn: (IceTipTableColumn new 
			id: 'Branch';
			action: #branchName;
			width: 150;
			yourself);
		addColumn: (IceTipTableColumn new 
			id: 'Status';
			action: #status;
			yourself);		
		dataSource: self newRepositoriesDataSource;
		selectRowIndex: 1;
		enableFilter: IceTipRepositoryFilter;			
		explicitFunction
]

{ #category : #initialization }
IceTipRepositoriesBrowser >> initializeWidgets [
	super initializeWidgets.
	
	repositoryList := self newIceTable.
	repositoryList widget
		onAnnouncement: FTStrongSelectionChanged 
		send: #repositoryStrongSelection: 
		to: self.

	self initializeRepositoryList.
	
	self focusOrder add: repositoryList 
]

{ #category : #'private factory' }
IceTipRepositoriesBrowser >> newContextWithSelection: anObject [ 
	^ self newContext 
		repositoryModel: anObject;
		yourself
]

{ #category : #'private factory' }
IceTipRepositoriesBrowser >> newRepositoriesDataSource [
	^ IceTipDataSource new
		tool: self;
		elements: self model repositories;
		yourself
]

{ #category : #private }
IceTipRepositoriesBrowser >> refresh [
	repositoryList widget 
		in: [ :this | this dataSource elements: self model repositories ];
		refresh
]

{ #category : #accessing }
IceTipRepositoriesBrowser >> repositoryList [
	^ repositoryList
]

{ #category : #accessing }
IceTipRepositoriesBrowser >> repositoryList: anObject [
	repositoryList := anObject
]

{ #category : #private }
IceTipRepositoriesBrowser >> repositorySelected [
	| selectedIndex |
	self flag: #todo. "This should -maybe- be modeled better on a SpecTablePresenter ;)"
	selectedIndex := repositoryList widget selectedRowIndex.
	selectedIndex = 0 ifTrue: [ ^ nil ].	
	^ repositoryList widget dataSource 
		elementAt: selectedIndex 
		ifAbsent: [ nil ]
]

{ #category : #private }
IceTipRepositoriesBrowser >> repositoryStrongSelection: ann [
	| selection context |
	
	selection := repositoryList widget dataSource elementAt: ann selectedRowIndex.
	context := self newContextWithSelection: selection.
	IceTipCommandStrongSelectionActivation
		activateAllInContext: context
		by: [ :each | each executeCommand ].		

]

{ #category : #initialization }
IceTipRepositoriesBrowser >> subscribeToAnnouncements [
	Iceberg announcer weak 
		when: IceAnnouncement send: #refresh to: self.

]

{ #category : #accessing }
IceTipRepositoriesBrowser >> title [
	^ 'Repositories'
]

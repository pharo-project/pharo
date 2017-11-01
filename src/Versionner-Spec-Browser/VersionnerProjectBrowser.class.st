"
A VersionnerProjectBrowser can be used to browse a Metacello configuration.
You can display and edit project versions and dependencies.

Quick overview:
VersionnerProjectBrowser example
"
Class {
	#name : #VersionnerProjectBrowser,
	#superclass : #ComposablePresenter,
	#instVars : [
		'allVersionsCheckBox',
		'configuration',
		'project',
		'projectPanel',
		'repository',
		'repositoryLabel',
		'repositoryAddButton',
		'toolbar',
		'versionCommitMessage',
		'versionDescription',
		'versionList',
		'versionsLabel',
		'versionDescriptionLabel'
	],
	#category : 'Versionner-Spec-Browser'
}

{ #category : #spec }
VersionnerProjectBrowser class >> defaultSpec [
	"Define the layout of the Versionner Browser"
	^ SpecLayout composed
		newColumn: [ : column | 
			column
				add: #toolbar height: self toolbarHeight;
				newRow: [ :mainPanel | mainPanel
					newColumn: [ :leftPanel | leftPanel
						newRow: [ :versionsHeader | 
							versionsHeader
								add: #versionsLabel;
								add: #allVersionsCheckBox ] 
						height: self toolbarHeight;					
						
						add: #versionList ] 
					right: 0.7;
					
					addSplitter;
					
					add: #projectPanel left: 0.3
				];
				newRow: [ :row |
					row
						newColumn: [  :col | 
							col
								newRow: [:repoHeader | repoHeader
									add: #repositoryLabel  width: 75;
									add: #repository;
									add: #repositoryAddButton width: self toolbarHeight ] 
								height: self toolbarHeight;
						
								newColumn: [:versionDescRow | versionDescRow 
									add: #versionDescriptionLabel height: self toolbarHeight;
									add: #versionDescription ];
								
								add: #versionCommitMessage height: self toolbarHeight ] ] height: 120
			
			];
	yourself.
 
]

{ #category : #examples }
VersionnerProjectBrowser class >> example [
	^ self
		openOn: (MBConfigurationRoot current configurations detect: [ :config | config name = 'ConfigurationOfOSWindow' ])
]

{ #category : #ui }
VersionnerProjectBrowser class >> openOn: configuration [
	^ self new
		configuration: configuration;
		createBaselineIfEmptyConfiguration;
		openWithSpec;
		yourself
]

{ #category : #accessing }
VersionnerProjectBrowser >> allVersionsCheckBox [
	^ allVersionsCheckBox
]

{ #category : #api }
VersionnerProjectBrowser >> configuration: aMBConfigurationInfo [ 
	configuration := aMBConfigurationInfo.
	configuration ifNotNil: [ 
		configuration recalculateSilently.
		self versionList: configuration versions ].
]

{ #category : #actions }
VersionnerProjectBrowser >> createBaselineIfEmptyConfiguration [
	configuration versions ifEmpty: 
		[ | tempProject|
		tempProject := MTProject new
						name: configuration name configurationBaseName;
						configurationClass: configuration;
						yourself.
		tempProject devWorkflow createInitialDevelopment ]
]

{ #category : #testing }
VersionnerProjectBrowser >> hasChanges [
	|  initialProject |
	
	versionList selectedItem ifNil: [ ^ false ].
	initialProject := MTProject 
						newFromVersion: versionList selectedItem version
						inConfiguration: configuration configurationClass.
	^ (MTModelComparator is: project equalsTo: initialProject) not
]

{ #category : #ui }
VersionnerProjectBrowser >> iconForVersion: aMBVersionInfo [ 
	^ self iconNamed: #databaseIcon
]

{ #category : #testing }
VersionnerProjectBrowser >> ignoreUnsavedChangesOnDevelopmentIfAny [
	^ self hasChanges
		ifTrue: [ self confirm: 'There are unsaved changes to the development version.
Ok to loose these changes?' ]
		ifFalse: [ true ]
]

{ #category : #initialization }
VersionnerProjectBrowser >> initialExtent [
	 ^  (900.0@650.0)
]

{ #category : #initialization }
VersionnerProjectBrowser >> initialize [ 
	super initialize.
	self register.
]

{ #category : #initialization }
VersionnerProjectBrowser >> initializePresenter [
	"Initialize widgets behavior"

	versionList
		whenSelectedItemChanged: [ :selection | 
			selection 
				ifNil: [ toolbar releaseEnabled: false ]
				ifNotNil: [ toolbar releaseEnabled: selection isDevelopment.
						versionList menu: [ :aMenu | self loadMenuFor: selection in: aMenu ] ].
			self refreshConfiguration.
			self update ];
		displayBlock: [ :item | VSVersionMorph withVersion: item ];
		yourself.

	allVersionsCheckBox
		whenChangedDo: [ :selector | | shouldUpdateList | 
							shouldUpdateList := self hasChanges
								ifTrue: [ self confirm: 'There are unsaved changes. Ok to loose these changes?' ]
								ifFalse: [ true ].
							shouldUpdateList ifTrue: [ self versionList: configuration versions ] ].
	
	repository acceptBlock: [ project repository: repository text ].
	
	repositoryAddButton action: [ self updateRepository ]
]

{ #category : #initialization }
VersionnerProjectBrowser >> initializeWidgets [
	"toolbar"
	toolbar := self instantiate: VersionnerProjectToolBar.
	toolbar browser: self.
	
	"repository"
	(repositoryLabel := self instantiate: LabelPresenter)
		label: 'Repository:'.
	(repository := self instantiate: TextInputFieldPresenter)
		entryCompletion: nil;
		acceptOnCR: true;
		ghostText: 'none'.
	repositoryAddButton := (self instantiate: ButtonPresenter)
		icon: (self iconNamed: #monticelloPackageIcon);
		help: 'Add a new repository to this project';
		yourself.
	
	"versions"
	versionList := (self instantiate: VersionListModel )
		icons: [:version | self iconForVersion: version ];
		on: PharoShortcuts current browseShortcut do: [ versionList selectedItem 
			ifNotNil: [ :selection |
				(VSBrowseVersionCommand target: selection version for: self) execute ] ];
		changedBlock: [ self hasChanges ];
		yourself.
	versionsLabel := (self instantiate: LabelPresenter)
		label: 'Versions';
		yourself.
	allVersionsCheckBox := (self instantiate: CheckBoxPresenter)
		label: 'browse all';
		state: false;
		yourself.

	"Version details"
	versionDescriptionLabel := (self instantiate: LabelPresenter)
		label: 'Version description:';
		yourself.
	versionDescription := (self instantiate: TextPresenter)
		enabled: false;
		yourself.
	versionCommitMessage := self instantiate: LabelPresenter.
	
	"Project details"
	projectPanel := self instantiate: VersionnerProjectPanel.
	
]

{ #category : #menu }
VersionnerProjectBrowser >> loadMenuFor: mbInfo in: aMenu [ 
	"dynamic menu"
	aMenu target: self.
	((mbInfo commands 
		collect: [ :commandClass |commandClass target: mbInfo for: (versionList selectedItem)]
		thenSelect: [ :each | each canBeApplied ])
		asArray sort: [:a :b | a order <= b order ]) 
			do: [:cmd | 
				aMenu 
					add: cmd title 
					target: [	
						cmd executeWithDevWorkflow: project devWorkflow.
						self refreshConfiguration.
						self update ]
					selector:#value ].
	^ aMenu.
]

{ #category : #accessing }
VersionnerProjectBrowser >> project [
	^ project
]

{ #category : #accessing }
VersionnerProjectBrowser >> projectPanel [
	^ projectPanel
]

{ #category : #refreshing }
VersionnerProjectBrowser >> refreshConfiguration [
	configuration
		ifNotNil: [ self configuration: (MBConfigurationRoot current configurationInfoFor: configuration configurationClass) ]
]

{ #category : #refreshing }
VersionnerProjectBrowser >> refreshConfigurationOfProject: anAnnouncement [
	configuration = anAnnouncement configurationInfo
		ifTrue: [ self refreshConfiguration ]
]

{ #category : #'announcement registration' }
VersionnerProjectBrowser >> register [
	MBConfigurationRoot current announcer weak
		when: MBConfigurationInfoChanged
		send: #refreshConfigurationOfProject:
		to: self
]

{ #category : #accessing }
VersionnerProjectBrowser >> repository [
	^ repository
]

{ #category : #accessing }
VersionnerProjectBrowser >> repositoryAddButton [
	^ repositoryAddButton
]

{ #category : #accessing }
VersionnerProjectBrowser >> repositoryLabel [
	^ repositoryLabel
]

{ #category : #refreshing }
VersionnerProjectBrowser >> selectDevelopmentVersion [
	versionList selectDevelopment
]

{ #category : #accessing }
VersionnerProjectBrowser >> title [
	^ configuration name configurationBaseName
]

{ #category : #accessing }
VersionnerProjectBrowser >> toolbar [

	^ toolbar.
]

{ #category : #'announcement registration' }
VersionnerProjectBrowser >> unregister [
	"Unregister from the dependent object. Typically used when closing the browser"
	
	MBConfigurationRoot current announcer unsubscribe: self  
]

{ #category : #refreshing }
VersionnerProjectBrowser >> update [
	"Update the project details according to selected configuration and version."

	versionList selectedItem
		ifNil: [ projectPanel project: nil ]
		ifNotNil: [ :selectedVersion | 
			project := MTProject 
						newFromVersion: selectedVersion version
						inConfiguration: configuration configurationClass.
			projectPanel project: project.					
			repository text: project repository.
			versionDescription text: project version description.
			versionCommitMessage label: 'Commited: ' , project version author , ' - ' , project version timestamp ]
]

{ #category : #actions }
VersionnerProjectBrowser >> updateRepository [
	"Update the core repository of this project"
	| package repoManager |
	
	project ifNotNil: [ 
		package := project configurationClass package mcPackage.
		repoManager := VersionnerRepositoriesManager new.
		repoManager
			selectedChangedBlock:  [:remote :selected |
				selected
					ifTrue: [ | repositoryGroup | 
							repositoryGroup := package workingCopy repositoryGroup.
							repositoryGroup repositoriesDo: [ :aRepository | 
									(aRepository location = repository text) 
										ifTrue: [ repositoryGroup removeRepository: aRepository ] ].
							repository accept: remote location.
							repositoryGroup addRepository: remote ] ];
			open;
			yourself  ]
]

{ #category : #accessing }
VersionnerProjectBrowser >> versionCommitMessage [
	^ versionCommitMessage
]

{ #category : #accessing }
VersionnerProjectBrowser >> versionDescription [
	^ versionDescription
]

{ #category : #accessing }
VersionnerProjectBrowser >> versionDescriptionLabel [
	^ versionDescriptionLabel
]

{ #category : #accessing }
VersionnerProjectBrowser >> versionList [
	^ versionList.
]

{ #category : #accessing }
VersionnerProjectBrowser >> versionList: aVersionList [
	"Select versions to display and give them to the list model."

	| selectedVersions |
	selectedVersions := allVersionsCheckBox state
		ifTrue: [ aVersionList ]
		ifFalse: [ 
			aVersionList
				select: [ :version | version isStable or: [ version isDevelopment or: [ version isCurrentVersion or: [ version isBleedingEdge ] ] ] ] ].
	self versionList items: selectedVersions
]

{ #category : #accessing }
VersionnerProjectBrowser >> versionsLabel [
	^ versionsLabel
]

{ #category : #ui }
VersionnerProjectBrowser >> windowIsClosing [
	self unregister.
]

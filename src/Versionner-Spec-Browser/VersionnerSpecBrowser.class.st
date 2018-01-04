"
Tool used to manage project dependencies.
This tool uses ToolBox and Metacello to manage project dependencies (ConfigurationOf*).
The GUI is built with Spec.

You can find Versionner in the World menu (Tools category).
Otherwise, you can launch it with : 
  VersionnerSpecBrowser open.

"
Class {
	#name : #VersionnerSpecBrowser,
	#superclass : #ComposablePresenter,
	#instVars : [
		'toolbar',
		'configurationList'
	],
	#category : #'Versionner-Spec-Browser'
}

{ #category : #ui }
VersionnerSpecBrowser class >> clean [
	self allInstances do:[ :a| a unregister].
	Smalltalk garbageCollect.
]

{ #category : #icons }
VersionnerSpecBrowser class >> configurationChangedIcon [
	^ self iconNamed: #configDirty
]

{ #category : #icons }
VersionnerSpecBrowser class >> configurationLoadedIcon [
	^ self iconNamed: #configLoaded
]

{ #category : #spec }
VersionnerSpecBrowser class >> defaultSpec [
	"Define the layout of the Versionner Browser"
	^ SpecLayout composed
		newColumn: [ :column | 
			column
				add: #toolbar height: self toolbarHeight;
				add: #configurationList ];
	yourself.
 
]

{ #category : #configurations }
VersionnerSpecBrowser class >> imageConfigurations [
	| configs |
	
	configs := MBConfigurationRoot current configurations.
	configs := configs reject: [ :config | self testConfigurations includes: config name ].
	^ configs
]

{ #category : #ui }
VersionnerSpecBrowser class >> menuCommandOn: aBuilder [
	<worldMenu>
	(aBuilder item: #VersionnerSpecBrowser)
		order: 0.21;
		parent: #Tools;
		label: 'Versionner';
		iconName: #databaseIcon;
		help: 'Create and edit project configurations for code versionned via Monticello.';
		action: [ VersionnerSpecBrowser open ]
]

{ #category : #ui }
VersionnerSpecBrowser class >> open [
	|windows|
	windows := self openOn: self imageConfigurations.
	^ windows.
]

{ #category : #ui }
VersionnerSpecBrowser class >> openOn: configurations [
	| browser |
	browser := self new. 
	browser configurationList: configurations.
	browser openWithSpec.
	^ browser.
]

{ #category : #settings }
VersionnerSpecBrowser class >> settingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #versionner)
		label: 'Versionner';
		parent: #'SCM';
		description: 'Settings related to Versionner the tool to manage projects and configurations'
]

{ #category : #configurations }
VersionnerSpecBrowser class >> testConfigurations [
	^ #( 'ConfigurationOfVersionnerTestBitmapCharacterSet' . 
		'ConfigurationOfVersionnerTestXMLParserTemplate' .
		'ConfigurationOfVersionnerTestXMLWriter' .
		'MetacelloBaseConfiguration' .
		'MetacelloExampleProjectConfig' .
		'MetacelloMCTestsAbstractConfig' )
]

{ #category : #api }
VersionnerSpecBrowser >> aboutText [
	^aboutText value: 'Versionner is a GUI tool for working with Metacello configurations, which manage the dependencies between packages and projects. While it does not cover Metacello''s full API, it makes many common cases much easier than writing configurations by hand.

For a tutorial video on how to use Versionner:
	https://youtu.be/cFRJDuWL-Q0'
]

{ #category : #api }
VersionnerSpecBrowser >> aboutTitle [
	^ self toolName.
]

{ #category : #api }
VersionnerSpecBrowser >> browseConfiguration: aConfiguration [
	aConfiguration recalculate.
	^ VersionnerProjectBrowser openOn: aConfiguration.
]

{ #category : #accessing }
VersionnerSpecBrowser >> configurationList [
	
	^ configurationList
]

{ #category : #accessing }
VersionnerSpecBrowser >> configurationList: aList [
	"Here I reroute my entry point to the list model's entry point"
	self configurationList items: aList.
]

{ #category : #accessing }
VersionnerSpecBrowser >> iconForConfiguration: aConfiguration [

	"should return an IconMorph"
	^ (aConfiguration isDirty)
					ifTrue:  [ self class configurationChangedIcon] 
					ifFalse: [ self class configurationLoadedIcon ].
]

{ #category : #initialization }
VersionnerSpecBrowser >> initialExtent [

	 ^  (400.0@500.0)
]

{ #category : #initialization }
VersionnerSpecBrowser >> initialize [ 
	super initialize.
	self register.
]

{ #category : #initialization }
VersionnerSpecBrowser >> initializePresenter [
	"Initialize widgets behavior"
	configurationList
		whenSelectedItemChanged: [ :selection | 
			selection
				ifNil: [ toolbar openButton enabled: false ]
				ifNotNil: [ toolbar openButton enabled: true.
						configurationList menu: [ :aMenu | self loadMenuFor: selection in: aMenu ] ] ].
			
]

{ #category : #initialization }
VersionnerSpecBrowser >> initializeWidgets [
	"configurations"
	configurationList := (self instantiate: FastTablePresenter)
		icons: [ :configuration | self iconForConfiguration: configuration ];
		displayBlock: [:item | item asString ]; 
		"iconMaxSize: 16@16;"
		handlesDoubleClick: true;
		on: PharoShortcuts current browseShortcut 
			do: [ configurationList selectedItem 
					ifNotNil: [ :selection | selection configurationClass browse ] ];
		doubleClickAction: [ configurationList selectedItem ifNotNil: [ :selection | 
				self browseConfiguration: selection ] ];
		yourself.
	
	"toolbar"
	toolbar := self instantiate: VersionnerToolBar.
]

{ #category : #menu }
VersionnerSpecBrowser >> loadMenuFor: mbInfo in: aMenu [ 
	| commands menuGroup |
	
	"dynamic menu"
	commands := mbInfo commands collect: [ :commandClass | 
		commandClass 
			target: mbInfo 
			for: (configurationList selectedItem) ].
	commands := commands sorted: [:a :b | a order <= b order].
	
	menuGroup := MenuGroupPresenter new.
	commands do: [ :command |
		menuGroup addMenuItem: 
			(MenuItemPresenter new
				name: command title;
				action: [ self unregister.
						 command execute.
						 self register ]) ].
	menuGroup buildWithSpec do: [ :eachMenuItem |
		aMenu addMenuItem: eachMenuItem ].
	^aMenu
]

{ #category : #actions }
VersionnerSpecBrowser >> refresh [
	self configurationList: self class imageConfigurations
]

{ #category : #initialization }
VersionnerSpecBrowser >> register [
	"Set up dependencies"
	
	MBConfigurationRoot current announcer weak
		when: MBConfigurationInfoCreated send: #refresh to: self;
		when: MBConfigurationInfoChanged send: #refresh to: self;
		when: MBConfigurationInfoDeleted send: #refresh to: self;
		when: MBInfoListChanged send: #refresh to: self.
	SystemAnnouncer uniqueInstance weak
		when: MCPackageModified send: #refresh to: self.

]

{ #category : #accessing }
VersionnerSpecBrowser >> title [
	^ self toolName , ' - Configuration list'
]

{ #category : #accessing }
VersionnerSpecBrowser >> toolName [
	^ 'Versionner'
]

{ #category : #accessing }
VersionnerSpecBrowser >> toolbar [

	^ toolbar.
]

{ #category : #initialization }
VersionnerSpecBrowser >> unregister [
	"Unregister from the dependent object. Typically used when closing the browser"
	
	MBConfigurationRoot current announcer unsubscribe: self.
	SystemAnnouncer uniqueInstance unsubscribe: self
]

{ #category : #actions }
VersionnerSpecBrowser >> windowIsClosing [
	self unregister.
]

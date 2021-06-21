"
I provide the ability to: 
* select a plugin to create/run tests
* select sets of items to analyze
* obtain a detailed log of the results 

UI Description 
___________ 

The droplist contains all the plugins available to start the analysis. 

My left-most pane lists all of the categories that contain items (could subclasses of TestCase, executable comments, etc.); Once items are selected, the items that can be analyzed appear in the pane to right.
The right-most pane shows the results in different groups, depends the plugin's analysis. 

Run and browse buttons behaviour are defined by the current plugin selected.


"
Class {
	#name : #DrTests,
	#superclass : #AbstractDrTestsPresenter,
	#instVars : [
		'pluginsDropList',
		'statusLabel',
		'plugins',
		'pluginPresenter'
	],
	#category : #'DrTests-Spec'
}

{ #category : #'tools registry' }
DrTests class >> beDefaultTestRunner [
	<script>
	
	self registerToolsOn: Smalltalk tools
]

{ #category : #specs }
DrTests class >> defaultSpec [

	^ SpBoxLayout newTopToBottom
		add:
			(SpBoxLayout newLeftToRight
				add: #pluginsDropList;
				add: #switchButton
					withConstraints: [ :constraints | constraints width: self buttonWidth * 2 ];
				yourself)
			withConstraints: [ :constraints | 
				constraints
					expand: false;
					height: self toolbarHeight ];
		add: #pluginPresenter;
		yourself
]

{ #category : #'world menu' }
DrTests class >> menuCommandOn: aBuilder [
	<worldMenu>
	
	(aBuilder item: #'Dr Test (Preview)')
		parent: #Testing;
		action: [ self open ];
		order: 2;
		"keyText: 'o, u';" "Note: Removed the shortcut to not clash with SUnitRunner keybinding."
		help: '(Preview) Let you run and debug SUnit tests.';
		icon: self taskbarIcon;
		withSeparatorAfter	
]

{ #category : #api }
DrTests class >> open [
	<script>
	
	^ self new
		openWithSpec;
		yourself
]

{ #category : #'tools registry' }
DrTests class >> registerToolsOn: registry [
	"Note: For now, DrTests is only available for preview, thus the following code is commented."
	
	"Add DrTests to registry to replace the old test runner." 
	"registry register: self as: #testRunner"
]

{ #category : #'instance creation' }
DrTests class >> reloadConfiguration: aDTPluginConfiguration withResults: aDTPluginResult andPlugin: aDTPlugin [

	self open
		updateStatus: 'Tests finished.';
		currentPlugin: aDTPlugin;
		testsConfiguration: aDTPluginConfiguration;
		updateWithPluginResult: aDTPluginResult
]

{ #category : #initialization }
DrTests >> connectPresenters [

	super connectPresenters.
	self pluginsDropList selectIndex: 1
]

{ #category : #accessing }
DrTests >> currentPlugin: aPlugin [

	super currentPlugin: aPlugin.
	pluginPresenter := self instantiate: aPlugin pluginPresenterClass on: {aPlugin. self}.
	
	self rebuildLayout.
	
	self updateStatus: aPlugin pluginName , ' plugin is ready to work!' translated
]

{ #category : #private }
DrTests >> dateAndTimeString [

	^ DateAndTime now asStringYMDHM
]

{ #category : #'private - initialization' }
DrTests >> initializeButtons [

	super initializeButtons.

	switchButton
		label: 'Minimize';
		disable.
]

{ #category : #'private - initialization' }
DrTests >> initializePluginPresenter [

	pluginPresenter := self instantiate: DTNullPluginPresenter
]

{ #category : #'private - initialization' }
DrTests >> initializePluginsDropList [

	pluginsDropList := self newDropList.
	self pluginsDropList
		help: 'Select the plugin used by Dr Tests UI.';
		items: self plugins;
		display: [ :pluginClass | pluginClass pluginName ];
		iconBlock: [ :pluginClass | pluginClass pluginIcon ];
		whenSelectedItemChangedDo: [ :pluginClass | self currentPlugin: pluginClass new ]
]

{ #category : #initialization }
DrTests >> initializePresenters [

	super initializePresenters.
		
	self
		initializeStatusLabel;
		initializePluginsDropList;
		initializePluginPresenter
]

{ #category : #'private - initialization' }
DrTests >> initializeStatusLabel [

	statusLabel := self newLabel
]

{ #category : #initialization }
DrTests >> initializeWindow: aWindowPresenter [

	aWindowPresenter
		title:  self title;
		initialExtent: 700 @ 500;
		statusBar: statusLabel
]

{ #category : #private }
DrTests >> menuForSelectedResult: menu [

	self resultSelected ifNil: [ ^ self ].
	self resultSelected content drTestsBuildContextMenu: menu
]

{ #category : #accessing }
DrTests >> packagesSelected [
	
	^ pluginPresenter packagesSelected 
]

{ #category : #accessing }
DrTests >> pluginPresenter [
	^ pluginPresenter
]

{ #category : #accessing }
DrTests >> plugins [

	^ plugins ifNil: [ DrTestsPlugin availablePlugins ]
]

{ #category : #'accessing - subwidgets' }
DrTests >> pluginsDropList [

	^ pluginsDropList
]

{ #category : #accessing }
DrTests >> selectedItems [
	
	^ pluginPresenter selectedItems 
]

{ #category : #'accessing - model' }
DrTests >> setModelBeforeInitialization: aDomainObject [

	plugins := aDomainObject
]

{ #category : #'accessing - subwidgets' }
DrTests >> statusLabel [

	^ statusLabel
]

{ #category : #'api - locking' }
DrTests >> subwidgetsToLock [

	^ pluginPresenter subwidgetsToLock 
]

{ #category : #'private - actions' }
DrTests >> switchToMiniDrTest [

	(MiniDrTests new
		testsConfiguration: self testsConfiguration;
		currentPlugin: self currentPlugin;
		updateWithPluginResult:self pluginResult ;
		yourself) openWithSpec
]

{ #category : #actions }
DrTests >> switchUI [

	super switchUI.
	self switchToMiniDrTest
]

{ #category : #accessing }
DrTests >> testsConfiguration [

	testsConfiguration := self currentPlugin buildConfigurationFrom: self.
	^ testsConfiguration
]

{ #category : #accessing }
DrTests >> testsConfiguration: aDTTestsConfiguration [

	self pluginPresenter packagesList selectItems: aDTTestsConfiguration packagesSelected.
	self pluginPresenter itemsList selectItems: aDTTestsConfiguration items
]

{ #category : #accessing }
DrTests >> title [

	^ 'Dr Tests - ' , currentPlugin pluginName
]

{ #category : #updating }
DrTests >> updateStatus: aString [
	self statusLabel
		label: ('{1}: {2}' format: { self dateAndTimeString . aString }).

]

{ #category : #updating }
DrTests >> updateSwitchButton: itemsSelected [
	currentPlugin allowMiniDrTests
		ifTrue: [ switchButton enabled: itemsSelected isNotEmpty ]
		ifFalse: switchButton disable
]

{ #category : #updating }
DrTests >> updateUI [
	super updateUI.
	
	pluginPresenter updateUI.

]

{ #category : #updating }
DrTests >> updateWithResults: results [

	pluginResult := results.
	pluginPresenter updateWithResults: results
]

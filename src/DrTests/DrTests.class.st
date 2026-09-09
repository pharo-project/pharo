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
	#name : 'DrTests',
	#superclass : 'AbstractDrTestsPresenter',
	#instVars : [
		'pluginsDropList',
		'statusLabel',
		'plugins',
		'pluginPresenter'
	],
	#category : 'DrTests-Spec',
	#package : 'DrTests',
	#tag : 'Spec'
}

{ #category : 'tools registry' }
DrTests class >> beDefaultTestRunner [
	<script>
	
	self initialize
	
]

{ #category : 'api' }
DrTests class >> defaultPreferredExtent [ 

	^  800@600
]

{ #category : 'class initialization' }
DrTests class >> initialize [

	self currentApplication register: self as: #testRunner
]

{ #category : 'world menu' }
DrTests class >> menuCommandOn: aBuilder [

	<worldMenu>
	(aBuilder item: #'Dr Test')
		parent: #Testing;
		action: [ self open ];
		order: 2;
		keyText: 'o, u';
		help: 'Let you run and debug SUnit tests.';
		iconName: self taskbarIconName;
		withSeparatorAfter
]

{ #category : 'api' }
DrTests class >> open [
	<script>

	^ (self newApplication: StPharoApplication current) 
		  open;
		  yourself
]

{ #category : 'tools registry' }
DrTests class >> registerToolsOn: registry [

	registry register: self as: #testRunner

]

{ #category : 'initialization' }
DrTests >> connectPresenters [

	"Take the first element in list"
	self pluginsDropList selectIndex: 1.
	self currentPlugin: self pluginsDropList selectedItem new
]

{ #category : 'accessing - subwidgets' }
DrTests >> contentForReRun [

	^ self pluginPresenter resultSelected contentForReRun
]

{ #category : 'accessing' }
DrTests >> currentPlugin: aPlugin [
	| newPluginPresenter |

	self currentPlugin = aPlugin ifTrue: [ ^ self ].

	super currentPlugin: aPlugin.

	"Always instantiate and update the plugin, as the plugin presenter class is not sufficient to tell them appart"
	newPluginPresenter := self
		instantiate: aPlugin pluginPresenterClass
		on: { aPlugin. self }.
	self layout
		replace: pluginPresenter
		with: newPluginPresenter.
	pluginPresenter := newPluginPresenter.

	self updateStatus: aPlugin pluginName , ' plugin is ready to work!' translated
]

{ #category : 'private' }
DrTests >> dateAndTimeString [

	^ DateAndTime now asStringYMDHM
]

{ #category : 'layout' }
DrTests >> defaultLayout [

	^ SpBoxLayout newTopToBottom
		spacing: 5;
		add: (SpBoxLayout newLeftToRight
				spacing: 5;
				add: pluginsDropList;
				add: switchButton expand: false;
				yourself)
			expand: false;
		add: pluginPresenter;
		add: statusLabel expand: false;
		yourself
]

{ #category : 'initialization' }
DrTests >> initializeButtons [

	super initializeButtons.
	switchButton
		label: 'Minimize';
		disable
]

{ #category : 'initialization' }
DrTests >> initializePluginPresenter [

	pluginPresenter := self instantiate: DTNullPluginPresenter
]

{ #category : 'initialization' }
DrTests >> initializePluginsDropList [

	pluginsDropList := self newDropList.
	self pluginsDropList
		help: 'Select the plugin used by Dr Tests UI.';
		items: self plugins;
		display: [ :pluginClass | pluginClass pluginName ];
		displayIcon: [ :pluginClass | pluginClass pluginIcon ];
		whenSelectedItemChangedDo: [ :pluginClass |
			self currentPlugin: pluginClass new ]
]

{ #category : 'initialization' }
DrTests >> initializePresenters [

	super initializePresenters.
	self
		initializeStatusLabel;
		initializePluginsDropList;
		initializePluginPresenter
]

{ #category : 'initialization' }
DrTests >> initializeStatusLabel [

	statusLabel := self newLabel
]

{ #category : 'private' }
DrTests >> menuForSelectedResult: menu [

	self resultSelected ifNil: [ ^ self ].
	self resultSelected content drTestsBuildContextMenu: menu
]

{ #category : 'accessing' }
DrTests >> packagesSelected [

	^ pluginPresenter packagesSelected
]

{ #category : 'accessing' }
DrTests >> pluginPresenter [
	^ pluginPresenter
]

{ #category : 'accessing' }
DrTests >> plugins [

	^ plugins ifNil: [ DrTestsPlugin availablePlugins ]
]

{ #category : 'accessing - subwidgets' }
DrTests >> pluginsDropList [

	^ pluginsDropList
]

{ #category : 'accessing' }
DrTests >> receivePluginPresenter: aPluginPresenter [

	self layout replace: pluginPresenter with: aPluginPresenter.
	pluginPresenter := aPluginPresenter
]

{ #category : 'initialization' }
DrTests >> reloadConfiguration: aDTPluginConfiguration withResults: aDTPluginResult andPlugin: aDTPlugin andPresenter: aDTPluginPresenter [
	
	self
		updateStatus: 'Tests finished.';
		currentPlugin: aDTPlugin;
		testsConfiguration: aDTPluginConfiguration;
		receivePluginPresenter: aDTPluginPresenter;
		updateWithPluginResult: aDTPluginResult
]

{ #category : 'accessing' }
DrTests >> selectedItems [

	^ pluginPresenter selectedItems
]

{ #category : 'accessing - model' }
DrTests >> setModelBeforeInitialization: aDomainObject [

	plugins := aDomainObject
]

{ #category : 'accessing - subwidgets' }
DrTests >> statusLabel [

	^ statusLabel
]

{ #category : 'api - locking' }
DrTests >> subwidgetsToLock [

	^ pluginPresenter subwidgetsToLock
]

{ #category : 'private - actions' }
DrTests >> switchToMiniDrTest [

	((MiniDrTests newApplication: self application)
		testsConfiguration: self testsConfiguration;
		currentPlugin: self currentPlugin;
		updateWithPluginResult: self pluginResult;
		receivePluginPresenter: self pluginPresenter;
		yourself)
		open
]

{ #category : 'actions' }
DrTests >> switchUI [

	super switchUI.
	self switchToMiniDrTest
]

{ #category : 'accessing' }
DrTests >> testsConfiguration [

	testsConfiguration := self currentPlugin buildConfigurationFrom: self.
	^ testsConfiguration
]

{ #category : 'accessing' }
DrTests >> testsConfiguration: aDTTestsConfiguration [

	self pluginPresenter packagesList selectItems: aDTTestsConfiguration packagesSelected.
	self pluginPresenter itemsList selectItems: aDTTestsConfiguration items
]

{ #category : 'updating' }
DrTests >> updateStatus: aString [
	self statusLabel
		label: ('{1}: {2}' format: { self dateAndTimeString . aString })
]

{ #category : 'updating' }
DrTests >> updateSwitchButton: itemsSelected [
	currentPlugin allowMiniDrTests
		ifTrue: [ switchButton enabled: itemsSelected isNotEmpty ]
		ifFalse: switchButton disable
]

{ #category : 'updating' }
DrTests >> updateUI [
	super updateUI.

	pluginPresenter updateUI
]

{ #category : 'updating' }
DrTests >> updateWithResults: results [

	pluginResult := results.
	pluginPresenter updateWithResults: results
]

{ #category : 'accessing' }
DrTests >> windowTitle [

	^ self currentPlugin 
		ifNotNil: [ :p | 'Dr Tests - ' , currentPlugin pluginName ]
		ifNil: [ 'Dr Tests' ]
]

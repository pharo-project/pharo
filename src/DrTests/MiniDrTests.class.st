"
A minified version of the DrTest tool.
"
Class {
	#name : 'MiniDrTests',
	#superclass : 'AbstractDrTestsPresenter',
	#instVars : [
		'startButton'
	],
	#category : 'DrTests-Spec',
	#package : 'DrTests',
	#tag : 'Spec'
}

{ #category : 'accessing' }
MiniDrTests >> currentPlugin: anObject [

	currentPlugin := anObject
]

{ #category : 'layout' }
MiniDrTests >> defaultLayout [

	^ SpBoxLayout newTopToBottom
		spacing: 5;
		add: switchButton expand: false;
		add: startButton;
		yourself
]

{ #category : 'accessing' }
MiniDrTests >> evaluateStyle [

	^ pluginResult backgroundColorStyle
]

{ #category : 'initialization' }
MiniDrTests >> initializeButtons [

	super initializeButtons.
	switchButton label: 'Maximize'.

	startButton := self newButton
		label: 'run';
		icon: (self iconNamed: #smallUpdate);
		action: [ self runPlugin ];
		yourself
]

{ #category : 'initialization' }
MiniDrTests >> initializeWindow: aWindowPresenter [

	super initializeWindow: aWindowPresenter.
	aWindowPresenter
		title: self title;
		initialExtent: 200@180;
		windowIcon: (self iconNamed: self class systemIconName)
]

{ #category : 'accessing' }
MiniDrTests >> startButton [
	^ startButton
]

{ #category : 'private - actions' }
MiniDrTests >> switchToDrTest [

	(DrTests newApplication: self application)
		reloadConfiguration: testsConfiguration
			withResults: pluginResult
			andPlugin: currentPlugin;
		open
]

{ #category : 'actions' }
MiniDrTests >> switchUI [

	super switchUI.
	self switchToDrTest
]

{ #category : 'accessing' }
MiniDrTests >> title [

	^ 'DrTests'
]

{ #category : 'updating' }
MiniDrTests >> updateUI [

	super updateUI.
	startButton styles copy
		do: [ :each | startButton removeStyle: each ].
	startButton addStyle: self evaluateStyle.
	startButton label: pluginResult summarizeInfo
]

{ #category : 'initialization' }
MiniDrTests >> updateWithResults: results [

	self pluginResult: results.
	self updateUI
]

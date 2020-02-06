Class {
	#name : #MiniDrTests,
	#superclass : #AbstractDrTestsUI,
	#instVars : [
		'resultsList',
		'pluginResult'
	],
	#category : #'DrTests-Spec'
}

{ #category : #specs }
MiniDrTests class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: (SpBoxLayout newHorizontal
				add: #switchButton;
				yourself)
		expand: false
		fill: true
		padding: 1;
		add: #startButton;
		yourself
]

{ #category : #accessing }
MiniDrTests >> currentPlugin: anObject [
	currentPlugin := anObject
]

{ #category : #accessing }
MiniDrTests >> evaluateBackgroundColor [
	^ pluginResult backgroundColor
]

{ #category : #initialization }
MiniDrTests >> initializeButtons [
	super initializeButtons.
	startButton
		label: 'run';
		icon: (self iconNamed: #smallUpdate);
		color: (Color gray).
	switchButton
		label: 'maxi Dr'
]

{ #category : #initialization }
MiniDrTests >> initializeWidgets [
	self initializeButtons
]

{ #category : #accessing }
MiniDrTests >> initializeWindow: aWindowPresenter [
	super initializeWindow: aWindowPresenter.
	aWindowPresenter
		title: self title;
		initialExtent: 200 @ 200
]

{ #category : #accessing }
MiniDrTests >> pluginResult [
	^ pluginResult
]

{ #category : #accessing }
MiniDrTests >> pluginResult: anObject [
	pluginResult := anObject
]

{ #category : #accessing }
MiniDrTests >> switchUI [
	super switchUI.
	DrTests
		reloadConfiguration: testsConfiguration
		withResults: pluginResult
		andPlugin: currentPlugin
]

{ #category : #accessing }
MiniDrTests >> title [
	^ 'MiniDrTests - ' , currentPlugin pluginName
]

{ #category : #accessing }
MiniDrTests >> updateUI [
	super updateUI.
	startButton color: self evaluateBackgroundColor.
	startButton label: pluginResult summarizeInfo.
	"The next line doesn't work because Spec2 does not handle button label's styles.
	Should be changed at some point"
	startButton label makeAllColor: pluginResult textColor.
]

{ #category : #initialization }
MiniDrTests >> updateWithPluginResult: aPluginResult [
	aPluginResult ifNotNil: [ self updateWithResults: aPluginResult ]
]

{ #category : #initialization }
MiniDrTests >> updateWithResults: results [
	self pluginResult: results.
	self updateUI
]

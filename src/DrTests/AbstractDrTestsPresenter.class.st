"
Abstract superclass for UI's of Dr Test
"
Class {
	#name : #AbstractDrTestsPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'pluginResult',
		'testsConfiguration',
		'currentPlugin',
		'switchButton'
	],
	#category : #'DrTests-Spec'
}

{ #category : #testing }
AbstractDrTestsPresenter class >> isAbstract [

	^ self name = #AbstractDrTestsPresenter
]

{ #category : #accessing }
AbstractDrTestsPresenter >> currentPlugin [

	^ currentPlugin
]

{ #category : #accessing }
AbstractDrTestsPresenter >> currentPlugin: anObject [
	"If a plugin was set before, unsubscribe first."

	currentPlugin ifNotNil: [ currentPlugin unconfigureUI: self ].
	currentPlugin := anObject.
	
	currentPlugin announcer
		when: DTStatusUpdate
		send: #handlePluginStatusUpdate:
		to: self.

	currentPlugin announcer
		when: DTUpdateResults
		send: #handlePluginResultUpdate:
		to: self.

]

{ #category : #events }
AbstractDrTestsPresenter >> handlePluginResultUpdate: resultsAnnouncement [ 

	self updateWithPluginResult: resultsAnnouncement results
]

{ #category : #'announcement handling' }
AbstractDrTestsPresenter >> handlePluginStatusUpdate: aDTStatusUpdate [

	self updateStatus: aDTStatusUpdate message
]

{ #category : #'private - initialization' }
AbstractDrTestsPresenter >> initializeButtons [

	switchButton := self newButton.
	switchButton
		icon: (self iconNamed: #smallRemoteOpen)
]

{ #category : #initialization }
AbstractDrTestsPresenter >> initializePresenter [

	self switchButton action: [ self switchUI ]
]

{ #category : #initialization }
AbstractDrTestsPresenter >> initializePresenters [

	self initializeButtons
]

{ #category : #'api - locking' }
AbstractDrTestsPresenter >> lock [
	"Lock the UI, once this method is called, the user is not able to click on buttons or lists anymore."
	
	self locked: false
]

{ #category : #'api - locking' }
AbstractDrTestsPresenter >> locked: aBoolean [
	"Lock or unlock widgets returned by #subwidgetsToLock depending on aBoolean."
	
	self subwidgetsToLock
		do: [ :subwidget | subwidget enabled: aBoolean ]
]

{ #category : #accessing }
AbstractDrTestsPresenter >> pluginResult [

	^ pluginResult
]

{ #category : #accessing }
AbstractDrTestsPresenter >> pluginResult: anObject [

	pluginResult := anObject
]

{ #category : #running }
AbstractDrTestsPresenter >> runPlugin [

	self runPluginFor: self testsConfiguration
]

{ #category : #running }
AbstractDrTestsPresenter >> runPluginFor: aTestsConfiguration [

	[ self
			lock;
			updateStatus: 'Tests started.';
			updateWithResults: (self currentPlugin runForConfiguration: aTestsConfiguration);
			updateStatus: 'Tests finished.' ] ensure: [ self unlock ]
]

{ #category : #'api - locking' }
AbstractDrTestsPresenter >> subwidgetsToLock [

	^ { self startButton }
]

{ #category : #accessing }
AbstractDrTestsPresenter >> switchButton [

	^ switchButton
]

{ #category : #accessing }
AbstractDrTestsPresenter >> switchButton: anObject [

	switchButton := anObject
]

{ #category : #'api - subwidgets configuration' }
AbstractDrTestsPresenter >> switchButtonAction: aBlock [

	self switchButton action: aBlock
]

{ #category : #actions }
AbstractDrTestsPresenter >> switchUI [

	self withWindowDo: [ :window | window close ]
]

{ #category : #accessing }
AbstractDrTestsPresenter >> testsConfiguration [

	^ testsConfiguration
]

{ #category : #accessing }
AbstractDrTestsPresenter >> testsConfiguration: anObject [

	testsConfiguration := anObject
]

{ #category : #'api - locking' }
AbstractDrTestsPresenter >> unlock [
	"Unlock the UI, once this method is called, the user is able to click on buttons or lists and to launch analysis."
	
	self locked: true
]

{ #category : #updating }
AbstractDrTestsPresenter >> updateStatus: aString [
	"Does nothing on purpose."
]

{ #category : #updating }
AbstractDrTestsPresenter >> updateUI [

	self withWindowDo: [ :window | window title: self title ]
]

{ #category : #updating }
AbstractDrTestsPresenter >> updateWithPluginResult: aPluginResult [

	aPluginResult ifNotNil: [ self updateWithResults: aPluginResult ]
]

{ #category : #updating }
AbstractDrTestsPresenter >> updateWithResults: results [

	self subclassResponsibility
]

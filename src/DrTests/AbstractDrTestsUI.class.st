Class {
	#name : #AbstractDrTestsUI,
	#superclass : #SpPresenter,
	#instVars : [
		'testsConfiguration',
		'startButton',
		'currentPlugin',
		'switchButton'
	],
	#category : #'DrTests-Spec'
}

{ #category : #testing }
AbstractDrTestsUI class >> isAbstract [
	^ AbstractDrTestsUI = self
]

{ #category : #accessing }
AbstractDrTestsUI >> currentPlugin [
	^ currentPlugin
]

{ #category : #accessing }
AbstractDrTestsUI >> currentPlugin: anObject [
	"If a plugin was set before, unsubscribe first."

	currentPlugin ifNotNil: [ currentPlugin announcer unsubscribe: self ].
	currentPlugin := anObject.
	currentPlugin announcer
		when: DTStatusUpdate
		send: #handlePluginStatusUpdate:
		to: self.
	self updateUI
]

{ #category : #'announcement handling' }
AbstractDrTestsUI >> handlePluginStatusUpdate: aDTStatusUpdate [
	self updateStatus: aDTStatusUpdate message
]

{ #category : #initialization }
AbstractDrTestsUI >> initializeButtons [
	startButton := self newButton.
	switchButton := self newButton.
	switchButton
		icon: (self iconNamed: #smallRemoteOpen)
]

{ #category : #accessing }
AbstractDrTestsUI >> initializePresenter [
	self startButton action: [ self runPlugin ].
	self switchButton action: [ self switchUI ]
]

{ #category : #'api - locking' }
AbstractDrTestsUI >> lock [
	"Lock the UI, once this method is called, the user is not able to click on buttons or lists anymore."
	self locked: false
]

{ #category : #'api - locking' }
AbstractDrTestsUI >> locked: aBoolean [
	"Lock or unlock widgets returned by #subwidgetsToLock depending on aBoolean."
	self subwidgetsToLock
		do: [ :subwidget | subwidget enabled: aBoolean ]
]

{ #category : #running }
AbstractDrTestsUI >> runPlugin [
	self runPluginFor: self testsConfiguration
]

{ #category : #running }
AbstractDrTestsUI >> runPluginFor: aTestsConfiguration [
	[ self lock.
	self updateStatus: 'Tests started.'.
	self
		updateWithResults: (self currentPlugin runForConfiguration: aTestsConfiguration).
	self updateStatus: 'Tests finished.' ]
		ensure: [ self unlock ]
]

{ #category : #'accessing - subwidgets' }
AbstractDrTestsUI >> startButton [
	^ startButton
]

{ #category : #'api - subwidgets configuration' }
AbstractDrTestsUI >> startButtonAction: aBlock [
	self startButton action: aBlock
]

{ #category : #'api - subwidgets configuration' }
AbstractDrTestsUI >> startButtonHelp: aString [
	self startButton help: aString
]

{ #category : #'api - subwidgets configuration' }
AbstractDrTestsUI >> startButtonLabel: aString [
	self startButton label: aString
]

{ #category : #'api - locking' }
AbstractDrTestsUI >> subwidgetsToLock [
	^ { self startButton }
]

{ #category : #accessing }
AbstractDrTestsUI >> switchButton [
	^ switchButton
]

{ #category : #accessing }
AbstractDrTestsUI >> switchButton: anObject [
	switchButton := anObject
]

{ #category : #'api - subwidgets configuration' }
AbstractDrTestsUI >> switchButtonAction: aBlock [
	self switchButton action: aBlock
]

{ #category : #'accessing - subwidgets' }
AbstractDrTestsUI >> switchUI [
	self withWindowDo: [ :window | window close ]
]

{ #category : #accessing }
AbstractDrTestsUI >> testsConfiguration [
	^ testsConfiguration
]

{ #category : #accessing }
AbstractDrTestsUI >> testsConfiguration: anObject [
	testsConfiguration := anObject
]

{ #category : #'api - locking' }
AbstractDrTestsUI >> unlock [
	"Unlock the UI, once this method is called, the user is able to click on buttons or lists and to launch analysis."
	self locked: true
]

{ #category : #updating }
AbstractDrTestsUI >> updateStatus: aString [
	"Does nothing on purpose."
]

{ #category : #updating }
AbstractDrTestsUI >> updateUI [
	^ self subclassResponsibility

]

{ #category : #updating }
AbstractDrTestsUI >> updateWithResults: results [
	self subclassResponsibility
]

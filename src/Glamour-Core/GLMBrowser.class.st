"
The Browser is one of the core components in Glamour.

It contains panes and transmissions between their ports. These transformations can either be explicitely defined by the user (such as in the Tabulator) or implicitely defined (such as in the Finder).

Browsers serve as composition managers. They determine when and under which conditions transmissions should be triggered and how they connect the ports of panes. In return, panes inform the browsers when event occur on their ports so that the browser can make an informed decission on what to do.

A Browser is a Presentation which means that it can be nested into other browsers.
"
Class {
	#name : #GLMBrowser,
	#superclass : #GLMPresentation,
	#instVars : [
		'transmissions',
		'panes',
		'initialExtent',
		'watcherPane'
	],
	#category : #'Glamour-Core'
}

{ #category : #constants }
GLMBrowser class >> defaultStatusbarPane [
	^ #'_statusbar'
]

{ #category : #scripting }
GLMBrowser class >> withStatusbar [
	^ self new addStatusbar
]

{ #category : #accessing }
GLMBrowser >> addPane: aPane [
	aPane browser: self.
	self panes add: aPane.
	self hasStatusbar ifTrue: [
		self addDefaultStatusbarTransmissionFrom: aPane ].
	^ aPane
]

{ #category : #accessing }
GLMBrowser >> addTransmission: aTransmission [ 
	aTransmission browser: self.
	^ self transmissions addLast: aTransmission
]

{ #category : #events }
GLMBrowser >> close [
	self announce: GLMBrowserClosed new
]

{ #category : #scripting }
GLMBrowser >> context [

	^OrderedCollection new
]

{ #category : #scripting }
GLMBrowser >> custom: aPresentation [ 
	 
	self subclassResponsibility
]

{ #category : #scripting }
GLMBrowser >> defaultRenderer [ 
	 
	^GLMRenderer subclasses first new
]

{ #category : #accessing }
GLMBrowser >> initialExtent [

	^ initialExtent ifNil: [ initialExtent := RealEstateAgent standardWindowExtent]
]

{ #category : #accessing }
GLMBrowser >> initialExtent: aPoint [

	initialExtent := aPoint
]

{ #category : #initialization }
GLMBrowser >> initialize [
	super initialize.
	panes := OrderedCollection new.
	transmissions := OrderedCollection new
]

{ #category : #scripting }
GLMBrowser >> initializeScriptingDefaults [ 
	"Initialize some default behavior if the user did not explcitly script it. 
 
	Subclasses can use this to create a default startup transmission or show 
	a default pane or something like that." 
	
	^self
]

{ #category : #accessing }
GLMBrowser >> lastActivePresentation: aPresentation [
	"this hook gets triggered every time, a presentation gets activated in a pane.
	by default the browser does nothing.
	specific browsers could use this hook to provide special behavior"
]

{ #category : #scripting }
GLMBrowser >> open [ 
	 
	^self openWith: self defaultRenderer
]

{ #category : #scripting }
GLMBrowser >> openOn: anObject [ 
	 
	^self 
		openOn: anObject 
		with: self defaultRenderer
]

{ #category : #scripting }
GLMBrowser >> openOn: anObject with: aRenderer [ 
	 
	self startOn: anObject. 
	^ self openWith: aRenderer
]

{ #category : #scripting }
GLMBrowser >> openWith: aRenderer [ 
	 
	^ aRenderer open: self
]

{ #category : #events }
GLMBrowser >> outerPortEvent: aPortEvent [  
	super outerPortEvent: aPortEvent.	
	aPortEvent hasChanged ifTrue: [
		(self transmissions select: [:each | 
			(each originatesAt: aPortEvent port) and: [
				( aPortEvent transmissionContext includesPort: each destination) not ]]) 
			do: [:each | each transmitIn: aPortEvent transmissionContext copy ]]
]

{ #category : #accessing }
GLMBrowser >> panes [
	^ panes ifNil: [	panes := OrderedCollection new ]
]

{ #category : #copying }
GLMBrowser >> postCopy [
	| newPane newPanes newPanesMapping newTransmission oldPane oldWatcherPane |
	super postCopy.
	"needed for setting the right panes for the new transmissions"
	newPanesMapping := Dictionary new. 
	newPanes := OrderedCollection new.
	self panes do: [:each | 
		newPane := each copy.
		newPane browser: self.
		newPanes addLast: newPane.
		newPanesMapping at: each put: newPane ].
	oldPane := self pane.
	oldWatcherPane := self watcherPane.
	watcherPane := nil.
	pane := nil.
	newPanesMapping at: oldPane ifAbsentPut: [ self pane ].
	newPanesMapping at: oldWatcherPane ifAbsentPut: [ self watcherPane ].
	self flag: 'we should also copy the values of ports from pane and watcherPane'.
	transmissions := self transmissions collect: [ :each | 
		newTransmission := each copy.
		newTransmission 
			changePortsAccordingToPaneMapping: newPanesMapping 
			fromOldBrowser: nil
			toNewBrowser: self.
		newTransmission ].
	panes := newPanes
]

{ #category : #accessing }
GLMBrowser >> removeAllPanes [
	self panes do: [:each | each unregisterFromAllAnnouncements].
	^ self panes removeAll
]

{ #category : #accessing }
GLMBrowser >> removeLastPane [
	self panes last unregisterFromAllAnnouncements.
	^ self panes removeLast
]

{ #category : #accessing }
GLMBrowser >> removePane: aPane [
	aPane unregisterFromAllAnnouncements.
	self panes remove: aPane
]

{ #category : #announcements }
GLMBrowser >> resetAnnouncer [
	super resetAnnouncer.
	self panes do: [:each | each resetAnnouncer ]
]

{ #category : #'scripting-private' }
GLMBrowser >> resolveDestinationPort: aPortReference [ 
	self flag: 'this is a temporary hack due to initializeScripting transmitting directly a port'.
	(aPortReference isKindOf: GLMPort) ifTrue: [ ^ aPortReference ].
	^self resolvePort: aPortReference asGlamourTargetIdentifier defaultPortName: #entity
]

{ #category : #'scripting-private' }
GLMBrowser >> resolveOriginPort: aPortReference [ 
	 
	^self resolvePort: aPortReference asGlamourOriginIdentifier defaultPortName: #selection
]

{ #category : #'scripting-private' }
GLMBrowser >> resolvePort: aPortReference defaultPortName: aSymbol [ 
	^ aPortReference paneName = #outer 
		ifTrue: [
			(GLMPresentationBoundPort new) 
						presentation: self; 
						name: aPortReference portName;
						yourself] 
		ifFalse: [
			(self paneNamed: aPortReference paneName) port: aPortReference portName].
]

{ #category : #scripting }
GLMBrowser >> startOn: anObject [ 
	 
	self initializeScriptingDefaults. 
	self entity: anObject
]

{ #category : #accessing }
GLMBrowser >> transmissions [ 
	^ transmissions
]

{ #category : #scripting }
GLMBrowser >> transmit [
	^ self addTransmission: (GLMTransmission new browser: self; yourself)
]

{ #category : #updating }
GLMBrowser >> unregisterFromAllAnnouncements [
	super unregisterFromAllAnnouncements.
	self panes do: [:each | 
		each unregisterFromAllAnnouncements ]
]

{ #category : #updating }
GLMBrowser >> update [ 
	self panes do: [:each | each update ].
	self announce: (GLMPresentationUpdated new presentation: self).
]

{ #category : #accessing }
GLMBrowser >> watcherPane [
	^ watcherPane ifNil: [watcherPane := GLMPane named: #'_watcher']
]

{ #category : #accessing }
GLMBrowser >> watcherPane: anObject [
	watcherPane := anObject 
]

{ #category : #enumerating }
GLMBrowser >> withAllPanesAndPresentationsDo: aBlock [
	super withAllPanesAndPresentationsDo: aBlock.
	self panes do: [:each | each withAllPanesAndPresentationsDo: aBlock ]
]

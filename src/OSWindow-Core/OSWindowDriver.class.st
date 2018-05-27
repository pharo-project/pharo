"
I am a basic (abstract) class, which represents an OS window driver.
The driver connects an OSWindow instances with underlaying operating system window(s) through managing OSWindowHandle(s). Driver provides an implemenation of all OSWindow functionality, starting from its creation, setting/retrieving its attributes, and finishing with event handling and/or rendering window's contents on screen.

The driver connects OSWindow(s) with operating system windows by providing the handle (see OSWindowHandle). The way how various OSWindow features and API are implemented is up to the concrete driver and thus considered private.

The driver(s) responsible for initial window creation , proper setup and and managing external resources. 
Again, most of driver's functionality is considered private and application-level code should not rely on any of its features. 

Driver selection mechanism:

 - on session change, i scan all of my subclasses to pick a most suitable driver which will be used on current platform (see #current on my class side)
"
Class {
	#name : #OSWindowDriver,
	#superclass : #Object,
	#classVars : [
		'Current',
		'Session'
	],
	#category : #'OSWindow-Core'
}

{ #category : #accessing }
OSWindowDriver class >> current [
	^ Session == Smalltalk session ifTrue: [ Current ifNil: [Current := self pickDriver ]]
		ifFalse: [ 
			Session := Smalltalk session.
			Current := self pickDriver
	]
	
]

{ #category : #'class initialization' }
OSWindowDriver class >> initialize [
	SessionManager default registerSystemClassNamed: self name
]

{ #category : #testing }
OSWindowDriver class >> isSuitable [
	"Answer true if receiver is most suitable for using in current session/platform"
	^ false
]

{ #category : #testing }
OSWindowDriver class >> isSupported [
	"Answer true if receiver is most suitable for using in current session/platform"
	^ false
]

{ #category : #'driver selection' }
OSWindowDriver class >> pickDriver [
	self subclassesDo: [ :s | s isSuitable ifTrue: [ ^ s new ] ].
	^ OSNullWindowDriver new
]

{ #category : #'system startup' }
OSWindowDriver class >> shutDown: quitting [
	quitting ifFalse: [ ^ self ].
	Current ifNil: [ ^ self ].

	"Clean driver"
	Current shutDown.
	Current := nil.
	
	"clean OSWindow worlds"
	WorldMorph extraWorldList copy 
		select: [ :each | each isKindOf: OSWindowWorldMorph ]
		thenDo: [ :each | WorldMorph removeExtraWorld: each ].
	"Clean also active world"
	(ActiveWorld isKindOf: OSWindowWorldMorph)
		ifTrue: [ ActiveWorld := nil ]
]

{ #category : #'window creation' }
OSWindowDriver >> createWindowWithAttributes: anOSWindowAttributes osWindow: osWindow [
	self subclassResponsibility
]

{ #category : #testing }
OSWindowDriver >> isNullDriver [
	^ false
]

{ #category : #'system startup' }
OSWindowDriver >> shutDown [
	"A hook to perform cleanups when shuting down a driver"
]

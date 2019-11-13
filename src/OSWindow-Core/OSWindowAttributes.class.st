"
My instances hold a number of various attributes, used during initial window creation and its initialization:

- initial bounds
- title
- icon
- screen id
- fullscreen or not
- etc...

Please note, that depending on driver used, some attributes can be ignored by driver and will have no any effect.  

The preferableDriver attribute is special, that it allows user to bypass the default driver selection mechanism and tell directly, which driver shall be used for creating a window.
"
Class {
	#name : #OSWindowAttributes,
	#superclass : #Object,
	#instVars : [
		'position',
		'extent',
		'title',
		'fullscreen',
		'screenId',
		'preferableDriver',
		'icon',
		'glAttributes',
		'windowCentered',
		'borderless',
		'resizable',
		'maximized',
		'minimized',
		'visible'
	],
	#classVars : [
		'DefaultBorderless',
		'DefaultExtent',
		'DefaultFullscreen',
		'DefaultMaximized',
		'DefaultMinimized',
		'DefaultPosition',
		'DefaultResizable',
		'DefaultTitle',
		'DefaultVisible',
		'DefaultWindowCentered'
	],
	#category : #'OSWindow-Core-Base'
}

{ #category : #defaults }
OSWindowAttributes class >> defaultBorderless [
	^ DefaultBorderless
]

{ #category : #defaults }
OSWindowAttributes class >> defaultExtent [
	^ DefaultExtent
]

{ #category : #defaults }
OSWindowAttributes class >> defaultFullscreen [
	^ DefaultFullscreen
]

{ #category : #defaults }
OSWindowAttributes class >> defaultMaximized [
	^ DefaultMaximized
]

{ #category : #defaults }
OSWindowAttributes class >> defaultMinimized [
	^ DefaultMinimized
]

{ #category : #defaults }
OSWindowAttributes class >> defaultPosition [
	^ DefaultPosition
]

{ #category : #defaults }
OSWindowAttributes class >> defaultResizable [
	^ DefaultResizable
]

{ #category : #defaults }
OSWindowAttributes class >> defaultTitle [
	^ DefaultTitle
]

{ #category : #defaults }
OSWindowAttributes class >> defaultVisible [
	^ DefaultVisible
]

{ #category : #defaults }
OSWindowAttributes class >> defaultWindowCentered [
	^ DefaultWindowCentered
]

{ #category : #'class initialization' }
OSWindowAttributes class >> initialize [
	"
	self initialize
	"
	DefaultPosition := 0@0.
	DefaultExtent :=150@150.
	DefaultFullscreen  := false.
	DefaultWindowCentered := false.
	DefaultTitle := 'New Window'.
	DefaultVisible := true.

	DefaultBorderless := false.
	DefaultResizable := true.
	DefaultMaximized := false.
	DefaultMinimized := false.
]

{ #category : #'instance creation' }
OSWindowAttributes class >> newWithNullDriver [
	^ self new preferableDriver: OSNullWindowDriver new; yourself
]

{ #category : #accessing }
OSWindowAttributes >> applyTo: window [
	window title: self title;
		icon: self icon.
	
]

{ #category : #accessing }
OSWindowAttributes >> borderless [
	^ borderless
]

{ #category : #accessing }
OSWindowAttributes >> borderless: anObject [
	borderless := anObject
]

{ #category : #accessing }
OSWindowAttributes >> bounds [
	^ position extent: extent
]

{ #category : #accessing }
OSWindowAttributes >> bounds: aRectangle [
	position := aRectangle origin.
	extent := aRectangle extent.
]

{ #category : #private }
OSWindowAttributes >> createWindowHandleFor: osWindow [
	^ (preferableDriver
		ifNil: [ preferableDriver := OSWindowDriver current ])
		createWindowWithAttributes: self
		osWindow: osWindow
]

{ #category : #accessing }
OSWindowAttributes >> extent [
	^ extent
]

{ #category : #accessing }
OSWindowAttributes >> extent: newExtent [
	extent := newExtent
]

{ #category : #accessing }
OSWindowAttributes >> fullscreen [
	
	^ fullscreen
]

{ #category : #accessing }
OSWindowAttributes >> fullscreen: anObject [
	
	fullscreen := anObject
]

{ #category : #accessing }
OSWindowAttributes >> glAttributes [
	
	^ glAttributes
]

{ #category : #accessing }
OSWindowAttributes >> glAttributes: anObject [
	
	glAttributes := anObject
]

{ #category : #accessing }
OSWindowAttributes >> height [
	^ extent y
]

{ #category : #accessing }
OSWindowAttributes >> icon [
	
	^ icon
]

{ #category : #accessing }
OSWindowAttributes >> icon: anObject [
	
	icon := anObject
]

{ #category : #initialization }
OSWindowAttributes >> initialize [
	visible := self class defaultVisible.
	position := self class defaultPosition.
	extent := self class defaultExtent.
	fullscreen := self class defaultFullscreen.
	title := self class defaultTitle.
	windowCentered := self class defaultWindowCentered.

	borderless := self class defaultBorderless.
	resizable := self class defaultResizable.
	minimized := self class defaultMinimized.
	maximized := self class defaultMaximized
]

{ #category : #accessing }
OSWindowAttributes >> maximized [
	^ maximized
]

{ #category : #accessing }
OSWindowAttributes >> maximized: aBoolean [
	maximized := aBoolean
]

{ #category : #accessing }
OSWindowAttributes >> minimized [
	^ minimized
]

{ #category : #accessing }
OSWindowAttributes >> minimized: aBoolean [
	minimized := aBoolean
]

{ #category : #accessing }
OSWindowAttributes >> position [
	^ position
]

{ #category : #accessing }
OSWindowAttributes >> position: aPoint [
	position := aPoint
]

{ #category : #accessing }
OSWindowAttributes >> preferableDriver [
	
	^ preferableDriver
]

{ #category : #accessing }
OSWindowAttributes >> preferableDriver: anObject [
	
	preferableDriver := anObject
]

{ #category : #accessing }
OSWindowAttributes >> resizable [
	^ resizable
]

{ #category : #accessing }
OSWindowAttributes >> resizable: aBoolean [
	resizable := aBoolean
]

{ #category : #accessing }
OSWindowAttributes >> screenId [
	^ screenId
]

{ #category : #accessing }
OSWindowAttributes >> screenId: newScreenId [
	screenId := newScreenId
]

{ #category : #accessing }
OSWindowAttributes >> title [
	
	^ title
]

{ #category : #accessing }
OSWindowAttributes >> title: anObject [
	
	title := anObject
]

{ #category : #accessing }
OSWindowAttributes >> visible [
	^ visible
]

{ #category : #accessing }
OSWindowAttributes >> visible: aBoolean [
	visible := aBoolean
]

{ #category : #accessing }
OSWindowAttributes >> width [
	^ extent x
]

{ #category : #accessing }
OSWindowAttributes >> windowCentered [
	^ windowCentered
]

{ #category : #accessing }
OSWindowAttributes >> windowCentered: aBoolean [
	windowCentered := aBoolean
]

{ #category : #accessing }
OSWindowAttributes >> x [
	^ position x
]

{ #category : #accessing }
OSWindowAttributes >> y [
	^ position y
]

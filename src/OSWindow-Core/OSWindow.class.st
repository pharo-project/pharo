"
I am a central class, which provides a top-level API for creating/controlling an operating-system windows.

To create a new OS window with default attributes, just use

OSWindow new.

For better control of window creation, use 

OSWindow createWithAttributes: ...

message. (See OSWindowAttributes for description).

A newly created OSWindow instance can be used and controlled by application.
To handle events (mouse/keyboard) of newly created window, one must 
bind own event handler to it (#eventHandler:) which must understand #handleEvent: message.

To render on window's surface, first application must obtain an OSWindowRenderer instance.
Currently there's two kinds of renderers available:
- form renderer (to use existing form for updating window's contents)
- opengl renderer (to render using OpenGL).

OSWindow instance and its handle: 
 - all operations with window (like hiding/showing/resizing etc) is possible only if its handle is valid. If window gets destroyed, or image opened in a new session while window was created in previous session, the handle becomes invalid, and any operations will lead to error. 
To test if window is still valid, you can just use #isValid message.
"
Class {
	#name : #OSWindow,
	#superclass : #Object,
	#instVars : [
		'handle',
		'initialAttributes',
		'eventHandler',
		'currentCursor'
	],
	#classVars : [
		'TraceEvents',
		'VMWindow'
	],
	#category : 'OSWindow-Core'
}

{ #category : #'clipboard handling' }
OSWindow class >> clipboardText [
	^ self validHandle clipboardText
]

{ #category : #'clipboard handling' }
OSWindow class >> clipboardText: aString [
	self validHandle clipboardText: aString
]

{ #category : #'instance creation' }
OSWindow class >> createWithAttributes: attributes [
	^ self basicNew initWithAttributes: attributes
]

{ #category : #finalization }
OSWindow class >> finalizeResourceData: windowHandle [
	windowHandle destroy.
]

{ #category : #'instance creation' }
OSWindow class >> new [
	^ self createWithAttributes: OSWindowAttributes new
]

{ #category : #'instance creation' }
OSWindow class >> newWithNullDriver [
	^ self createWithAttributes: OSWindowAttributes newWithNullDriver
]

{ #category : #tracing }
OSWindow class >> traceEvents: aBoolean [
	TraceEvents := aBoolean
]

{ #category : #accessing }
OSWindow >> borderless [
	^ self validHandle borderless
]

{ #category : #accessing }
OSWindow >> borderless: aBoolean [
	self validHandle borderless: aBoolean
]

{ #category : #accessing }
OSWindow >> bounds [
	^ self validHandle bounds
]

{ #category : #accessing }
OSWindow >> bounds: newBounds [
	self validHandle bounds: newBounds
]

{ #category : #'mouse capture' }
OSWindow >> captureMouse [
	self validHandle captureMouse
]

{ #category : #private }
OSWindow >> checkIsValid [

	(handle isNil or: [ handle isValid not ]) ifTrue: [ self invalidHandle ].
]

{ #category : #'clipboard handling' }
OSWindow >> clipboardText [
	^ self validHandle clipboardText
]

{ #category : #'clipboard handling' }
OSWindow >> clipboardText: aString [
	^ self validHandle clipboardText: aString
]

{ #category : #private }
OSWindow >> createWindow [
	handle ifNotNil: [ self error: 'The window is already created.' ].
	
	handle := initialAttributes createWindowHandleFor: self.
	handle isValid 
		ifTrue: [ FFIExternalResourceManager addResource: self data: handle ]
]

{ #category : #'dispatching events' }
OSWindow >> deliverEvent: anEvent [
	"TODO..."
	
	TraceEvents == true ifTrue: [ Transcript show: anEvent; cr ].
	eventHandler ifNotNil: [ 
		eventHandler handleEvent: anEvent ].
	anEvent performDefaultAction 

]

{ #category : #'dispatching events' }
OSWindow >> deliverGlobalEvent: aGlobalEvent [
	"TODO..."
	
	TraceEvents == true ifTrue: [ Transcript show: aGlobalEvent; cr ].
	eventHandler ifNotNil: [ 
		eventHandler handleEvent: aGlobalEvent ].

]

{ #category : #private }
OSWindow >> destroy [
	self validHandle destroy.
	handle := nil.
	eventHandler := nil.
]

{ #category : #accessing }
OSWindow >> eventHandler [
	
	^ eventHandler
]

{ #category : #accessing }
OSWindow >> eventHandler: anObject [
	
	eventHandler := anObject
]

{ #category : #accessing }
OSWindow >> extent [
	^ self validHandle extent
]

{ #category : #accessing }
OSWindow >> extent: newExtent [
	self validHandle extent: newExtent
]

{ #category : #accessing }
OSWindow >> fullscreen: aBoolean [
	self validHandle fullscreen: aBoolean
]

{ #category : #private }
OSWindow >> getFlags [
	^ self validHandle getFlags
]

{ #category : #accessing }
OSWindow >> handle [
	^ handle
]

{ #category : #visibility }
OSWindow >> hide [
	self checkIsValid .
	handle hide
]

{ #category : #accessing }
OSWindow >> icon: aForm [
	self validHandle icon: aForm
]

{ #category : #initialization }
OSWindow >> initWithAttributes: attributes [
	initialAttributes := attributes.
	self createWindow.
	self initialize
]

{ #category : #private }
OSWindow >> invalidHandle [
	self error: 'Invalid window handle'
]

{ #category : #testing }
OSWindow >> isValid [
	 ^ handle notNil and: [ handle isValid ]
]

{ #category : #testing }
OSWindow >> isVisible [
	^ self isValid and: [handle isVisible]
]

{ #category : #'window management' }
OSWindow >> maximize [

	self validHandle maximize.
]

{ #category : #'window management' }
OSWindow >> minimize [

	self validHandle minimize.
]

{ #category : #'instance creation' }
OSWindow >> newAthensRenderer [
	^ handle newAthensRenderer
]

{ #category : #factory }
OSWindow >> newFormRenderer: form [
	^ handle newFormRenderer: form
]

{ #category : #factory }
OSWindow >> newGenericRenderer [
	^ handle newGenericRenderer
]

{ #category : #factory }
OSWindow >> newOpenGLRenderer [
	^ handle newOpenGLRenderer
]

{ #category : #accessing }
OSWindow >> platformSpecificHandle [
	^ self validHandle platformSpecificHandle
]

{ #category : #accessing }
OSWindow >> position [
	^ self validHandle position
]

{ #category : #accessing }
OSWindow >> position: newPosition [
	self validHandle position: newPosition
]

{ #category : #'event handling' }
OSWindow >> processPendingEvents [
	handle processPendingEvents
]

{ #category : #'mouse capture' }
OSWindow >> releaseMouse [
	self validHandle releaseMouse
]

{ #category : #accessing }
OSWindow >> renderer [
	^ handle renderer
]

{ #category : #accessing }
OSWindow >> renderer: aRenderer [
	handle renderer: aRenderer
]

{ #category : #accessing }
OSWindow >> resizable [
	^ self validHandle resizable
]

{ #category : #accessing }
OSWindow >> resizable: aBoolean [
	self validHandle resizable: aBoolean
]

{ #category : #'window management' }
OSWindow >> restore [

	self validHandle restore.
]

{ #category : #'window management' }
OSWindow >> setDraggableArea: aRectangle [

	^self validHandle setDraggableArea: aRectangle.
]

{ #category : #accessing }
OSWindow >> setMouseCursor: cursorWithMask [
	currentCursor == cursorWithMask ifTrue: [ ^ self ].
	currentCursor := cursorWithMask.
	self validHandle setMouseCursor: cursorWithMask
]

{ #category : #accessing }
OSWindow >> setMouseCursor: cursor mask: mask [
	currentCursor == cursor ifTrue: [ ^ self ].
	currentCursor := cursor.

	self validHandle setMouseCursor: cursor mask: mask
]

{ #category : #visibility }
OSWindow >> show [
	self checkIsValid .
	handle show
]

{ #category : #accessing }
OSWindow >> title [
	^ self validHandle title
]

{ #category : #accessing }
OSWindow >> title: aString [
	self validHandle title: aString
]

{ #category : #'window management' }
OSWindow >> toggleBorderOff [

	self validHandle toggleBorderOff.
]

{ #category : #'window management' }
OSWindow >> toggleBorderOn [

	self validHandle toggleBorderOn.
]

{ #category : #private }
OSWindow >> validHandle [
	self checkIsValid.
	^ handle
]

{ #category : #accessing }
OSWindow >> windowId [
	^ self validHandle windowId
]

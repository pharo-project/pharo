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

Text input

Why does OSWindow need a text input API?
When I press a key on my keyboard, my program receives a character event, right?
Well, it's not always that simple. Sometimes it can take multiple key presses to produce a character. Sometimes a single key press can produce multiple characters.
Text input is not as simple as it seems, particularly when you consider International users (and you should). It's not hard to figure out why that is when you look at languages like Chinese, Japanese, and Korean. These languages, collectively referred to as the CJK, have thousands of symbols.
It would not be feasible to have a keyboard with over ten-thousand keys, would it? The solution to this is a software input method.

Related methods:
 - startTextInput
 - stopTextInput
 - isTextInputActive

(reference https://wiki.libsdl.org/Tutorials/TextInput)
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
	#category : #'OSWindow-Core-Base'
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
	^ self createWithAttributes: attributes eventHandler: nil
]

{ #category : #'instance creation' }
OSWindow class >> createWithAttributes: attributes eventHandler: eventHandler [
	^ self basicNew initWithAttributes: attributes eventHandler: eventHandler
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
	handle isValid ifTrue: [ 
		"Handle here points to an OSWindowHandle, its #handle message has the real 
		 external resource. This is a bit confusing, but is better like this :)"
		handle prepareExternalResourceForAutoRelease ]
]

{ #category : #'dispatching events' }
OSWindow >> deliverEvent: anEvent [
	"TODO..."

	TraceEvents == true
		ifTrue: [ self traceCr: anEvent ].
	eventHandler ifNotNil: [ eventHandler handleEvent: anEvent ].
	anEvent performDefaultAction
]

{ #category : #'dispatching events' }
OSWindow >> deliverGlobalEvent: aGlobalEvent [
	"TODO..."

	TraceEvents == true
		ifTrue: [ self traceCr: aGlobalEvent ].
	eventHandler ifNotNil: [ eventHandler handleEvent: aGlobalEvent ]
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

{ #category : #'window management' }
OSWindow >> focus [

	self validHandle raise.
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
OSWindow >> initWithAttributes: attributes eventHandler: anEventHandle [
	initialAttributes := attributes.
	eventHandler := anEventHandle.
	self createWindow.
	self initialize

]

{ #category : #private }
OSWindow >> invalidHandle [
	self error: 'Invalid window handle'
]

{ #category : #'text input' }
OSWindow >> isTextInputActive [
	"Check whether or not Unicode text input events are enabled"

	^ self validHandle isTextInputActive
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

{ #category : #private }
OSWindow >> setJustCreatedHandle: aHandle [
	"This method is to be called by a driver that needs to create the window in a critical section to prevent a race condition where some events such as expose could be missed."
	handle := aHandle
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

{ #category : #'text input' }
OSWindow >> startTextInput [
	"Start accepting Unicode text input events.
	I will start accepting Unicode text input events in the focused window, and start emitting text input and text editing events.
	Please use me in pair with stopTextInput.
	On some platforms I may activate the screen keyboard."

	self validHandle startTextInput
]

{ #category : #'text input' }
OSWindow >> stopTextInput [
	"Stop receiving any text input events"

	self validHandle stopTextInput
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

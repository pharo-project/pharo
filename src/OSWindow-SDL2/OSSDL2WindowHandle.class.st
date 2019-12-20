"
I am the OSWindowHandle interface implemented using SDL2 library

my handle is an SDL_Window
"
Class {
	#name : #OSSDL2WindowHandle,
	#superclass : #OSWindowHandle,
	#instVars : [
		'handle',
		'currentCursor',
		'iconSurface',
		'lastKeyboardFocus'
	],
	#pools : [
		'SDL2Constants'
	],
	#category : #'OSWindow-SDL2-Base'
}

{ #category : #'instance creation' }
OSSDL2WindowHandle class >> newWithHandle: handle attributes: attributes [
	^ self basicNew initWithHandle: handle attributes: attributes; yourself
]

{ #category : #accessing }
OSSDL2WindowHandle >> borderless [
	^ handle getFlags anyMask: SDL_WINDOW_BORDERLESS
]

{ #category : #accessing }
OSSDL2WindowHandle >> borderless: aBoolean [
	handle toggleBorder: aBoolean not.
]

{ #category : #accessing }
OSSDL2WindowHandle >> bounds: newBounds [
	self position: newBounds origin;
		extent: newBounds extent.
]

{ #category : #'mouse capture' }
OSSDL2WindowHandle >> captureMouse [
	SDL2 setRelativeMouseMode: true
]

{ #category : #accessing }
OSSDL2WindowHandle >> clipboardText [
	^ SDL2 clipboardText
]

{ #category : #accessing }
OSSDL2WindowHandle >> clipboardText: aText [
	SDL2 clipboardText: aText
]

{ #category : #'event handling' }
OSSDL2WindowHandle >> convertButtonState: mouseState modState: modState modifiers: modifiers [
	| shift ctrl alt ralt gui |
	modifiers buttons button1: (mouseState bitAnd: SDL_BUTTON_LMASK) ~= 0.
	modifiers buttons button2: (mouseState bitAnd: SDL_BUTTON_MMASK) ~= 0.
	modifiers buttons button3: (mouseState bitAnd: SDL_BUTTON_RMASK) ~= 0.
	modifiers buttons button4: (mouseState bitAnd: SDL_BUTTON_X1MASK) ~= 0.
	modifiers buttons button5: (mouseState bitAnd: SDL_BUTTON_X2MASK) ~= 0.

	shift := (modState bitAnd: KMOD_SHIFT) ~= 0.
	ctrl := (modState bitAnd: KMOD_CTRL) ~= 0.
	alt := (modState bitAnd: KMOD_LALT) ~= 0.
	ralt := (modState bitAnd: KMOD_RALT) ~= 0.
	gui := (modState bitAnd: KMOD_GUI) ~= 0.
    
	"Several keyboard layouts use AltGr to enter some symbols. In that case,
	the AltGr does not play role of a modifer and should be ignored. 
	As (hopefully) tempral solution, let's ignore all the modifiers if the AltGr is
	pressed (in this case, SDL adds Ctrl modifiers too).
	See https://github.com/pharo-project/pharo/issues/4745
	"
	ralt ifTrue: [ ^ self ].
    
	modifiers leftShift: shift; rightShift: shift;
		leftCtrl: ctrl; rightCtrl: ctrl;
		leftAlt: alt; rightAlt: ralt;
		leftCmd: gui; rightCmd: gui.
]

{ #category : #private }
OSSDL2WindowHandle >> convertCursor: aCursor [
	| result bits|
	aCursor unhibernate.
	bits := aCursor bits.
	result := ByteArray new: 32.
	0 to: 15 do: [ :i |
		result at: i*2 + 1 put: (bits at: i + 1) >> 24 & 255.
		result at: i*2 + 2 put: (bits at: i + 1) >> 16 & 255.
	].
	^ result
]

{ #category : #'event handling' }
OSSDL2WindowHandle >> deliverGlobalEvent: aGlobalEvent [
	^ osWindow deliverGlobalEvent: aGlobalEvent
]

{ #category : #initialize }
OSSDL2WindowHandle >> destroy [

	handle ifNotNil: [ :validHandle | 
		OSSDL2Driver current unregisterWindowWithId: validHandle windowID.
		validHandle destroy ].

	renderer := nil.
	handle := nil.
	osWindow := nil
]

{ #category : #accessing }
OSSDL2WindowHandle >> extent [
	| w h |
	w := ByteArray new: 4.
	h := ByteArray new: 4.
	handle getSizeW: w h: h.
	^ (w signedLongAt: 1) @ (h signedLongAt: 1)
]

{ #category : #accessing }
OSSDL2WindowHandle >> extent: newExtent [
	handle setSizeW: newExtent x h: newExtent y
]

{ #category : #accessing }
OSSDL2WindowHandle >> fullscreen: aBoolean [
	aBoolean ifTrue: [
		handle fullscreen: SDL_WINDOW_FULLSCREEN_DESKTOP
	] ifFalse: [ 
		handle fullscreen: 0
	]
]

{ #category : #private }
OSSDL2WindowHandle >> getFlags [
	^ handle getFlags
]

{ #category : #private }
OSSDL2WindowHandle >> getWMInfo [
	| wmInfo |
	wmInfo := SDL_SysWMinfo new version: SDL_Version bindingVersion.
	handle getWMInfo: wmInfo.
	^ wmInfo
]

{ #category : #initialize }
OSSDL2WindowHandle >> handle [

	^ handle
]

{ #category : #'event handling' }
OSSDL2WindowHandle >> handleNewSDLEvent: sdlEvent [
	^ sdlEvent accept: self
]

{ #category : #accessing }
OSSDL2WindowHandle >> hide [
	handle hide
]

{ #category : #accessing }
OSSDL2WindowHandle >> icon: aForm [
	| convertedIcon surface |
	aForm ifNil: [ ^self ].
	
	convertedIcon := aForm unhibernate; asFormOfDepth: 32.
	surface := SDL2 createRGBSurfaceFromPixels: convertedIcon bits
			width: convertedIcon width height: convertedIcon height
			depth: 32 pitch: convertedIcon width *4
			rmask: 16r00ff0000 
			gmask: 16r000ff00
			bmask: 16r00000ff
			amask: 16rff000000.
	handle icon: surface.
	iconSurface ifNotNil: [ SDL2 freeSurface: surface ].
	iconSurface := surface.
]

{ #category : #initialize }
OSSDL2WindowHandle >> initWithHandle: aHandle attributes: attributes [
	handle := aHandle.
	attributes applyTo: self
]

{ #category : #initialization }
OSSDL2WindowHandle >> initialize [
	
	self class initializeSlots: self.
	super initialize.
]

{ #category : #'text input' }
OSSDL2WindowHandle >> isTextInputActive [
	"See https://wiki.libsdl.org/SDL_IsTextInputActive"

	^ handle isTextInputActive
]

{ #category : #testing }
OSSDL2WindowHandle >> isValid [
	^ true
]

{ #category : #testing }
OSSDL2WindowHandle >> isVisible [
	^ (self getFlags bitAnd: SDL_WINDOW_SHOWN) = SDL_WINDOW_SHOWN
]

{ #category : #'event handling' }
OSSDL2WindowHandle >> mapSpecialCharacter: symbol [
	^ SDL2SpecialCharacterMapping mapKeySymbol: symbol ifAbsent: [ ^ nil ]
]

{ #category : #'window management' }
OSSDL2WindowHandle >> maximize [

	handle maximize.
]

{ #category : #'window management' }
OSSDL2WindowHandle >> minimize [

	handle minimize.
]

{ #category : #'event handling' }
OSSDL2WindowHandle >> mousePosition [
	| x y |
	x := ByteArray new: 4.
	y := ByteArray new: 4.
	SDL2 mouseStateX: x y: y.
	^ ( x signedLongAt: 1) @ (y signedLongAt: 1)
]

{ #category : #'instance creation' }
OSSDL2WindowHandle >> newAthensRenderer [
	^ renderer := OSSDL2AthensRenderer for: self.
]

{ #category : #'instance creation' }
OSSDL2WindowHandle >> newFormRenderer: form [
	^ renderer := OSSDL2FormRenderer new 
		form: form;
		window: self;
		yourself
]

{ #category : #'instance creation' }
OSSDL2WindowHandle >> newGenericRenderer [
	^ renderer := OSSDL2GenericRenderer new 
		window: self;
		createSDL2Renderer;
		yourself
]

{ #category : #'instance creation' }
OSSDL2WindowHandle >> newOpenGLRenderer [
	^ renderer := OSSDL2GLRenderer new 
		window: self;
		createContext;
		yourself
]

{ #category : #accessing }
OSSDL2WindowHandle >> platformSpecificHandle [
	| wmInfo platformID |
	wmInfo := self getWMInfo.
	platformID := wmInfo subsystem.
	platformID = SDL_SYSWM_WINDOWS ifTrue: [ ^ wmInfo info win asPlatformSpecificHandle ].
	platformID = SDL_SYSWM_X11 ifTrue: [ ^ wmInfo info x11 asPlatformSpecificHandle ].
	platformID = SDL_SYSWM_DIRECTFB ifTrue: [ ^ wmInfo info dfb asPlatformSpecificHandle ].
	platformID = SDL_SYSWM_COCOA ifTrue: [ ^ wmInfo info cocoa asPlatformSpecificHandle ].
	platformID = SDL_SYSWM_UIKIT ifTrue: [ ^ wmInfo info uikit asPlatformSpecificHandle ].
	platformID = SDL_SYSWM_WAYLAND ifTrue: [ ^ wmInfo info wl asPlatformSpecificHandle ].
	platformID = SDL_SYSWM_MIR ifTrue: [ ^ wmInfo info mir asPlatformSpecificHandle ].
	platformID = SDL_SYSWM_WINRT ifTrue: [ ^ wmInfo info winrt asPlatformSpecificHandle ].
	platformID = SDL_SYSWM_ANDROID ifTrue: [ ^ wmInfo info android asPlatformSpecificHandle ].
	
	self error: 'Trying to get a platform specific handle for an unsupported SDL2 platform.'
]

{ #category : #accessing }
OSSDL2WindowHandle >> position [
	| x y |
	x := ByteArray new: ExternalType long byteSize.
	y := ByteArray new: ExternalType long byteSize.
	handle getPositionX: x y: y.
	^ ( x signedLongAt: 1) @ (y signedLongAt: 1)
]

{ #category : #accessing }
OSSDL2WindowHandle >> position: aPoint [
	handle setPositionX: aPoint x asInteger y: aPoint y asInteger
]

{ #category : #accessing }
OSSDL2WindowHandle >> prepareExternalResourceForAutoRelease [
	handle autoRelease
]

{ #category : #accessing }
OSSDL2WindowHandle >> raise [

	handle raise
]

{ #category : #'mouse capture' }
OSSDL2WindowHandle >> releaseMouse [
	SDL2 setRelativeMouseMode: false
]

{ #category : #private }
OSSDL2WindowHandle >> resetKeyboardFocus [
		
	ActiveHand ifNil: [ ^ self ].
	lastKeyboardFocus := ActiveHand keyboardFocus.
	ActiveHand keyboardFocus: nil
]

{ #category : #accessing }
OSSDL2WindowHandle >> resizable [
	^ handle getFlags anyMask: SDL_WINDOW_RESIZABLE
]

{ #category : #accessing }
OSSDL2WindowHandle >> resizable: aBoolean [
	"This is not supported."
]

{ #category : #'window management' }
OSSDL2WindowHandle >> restore [

	handle restore
]

{ #category : #private }
OSSDL2WindowHandle >> restoreKeyboardFocus [

	(handle isNil 
		or: [ lastKeyboardFocus isNil 
		or: [ ActiveHand isNil ] ])
		ifTrue: [ ^ self ].
	
	[ ActiveHand keyboardFocus: lastKeyboardFocus ]
	ensure: [ 
		lastKeyboardFocus := nil ]
]

{ #category : #'window management' }
OSSDL2WindowHandle >> setDraggableArea: aRectangle [

	| myCallback sdlRect |
	
	sdlRect := aRectangle asSDLRect.
	
	myCallback := DraggableCallback on: [ :aWindow :pt :data |
			(pt x > sdlRect x) & (pt x < sdlRect w) & (pt y > sdlRect y) & (pt y < sdlRect h) ifTrue: [ 1 ] ifFalse: [ 0 ].
		].
	
	^handle setHitTest: myCallback.
]

{ #category : #cursor }
OSSDL2WindowHandle >> setMouseCursor: cursor mask: mask [
	| cursorBits maskBits extent offset sdlCursor |
	cursorBits := self convertCursor: cursor.
	maskBits := self convertCursor: mask.
	extent := cursor extent.
	offset := cursor offset.
	sdlCursor := SDL2 createCursor: cursorBits mask: maskBits w: extent x h: extent y hotX: offset x negated hotY: offset y negated.
	sdlCursor ifNotNil: [ sdlCursor setCursor ].
	currentCursor ifNotNil: [ currentCursor freeCursor ].
	currentCursor := sdlCursor
]

{ #category : #accessing }
OSSDL2WindowHandle >> show [
	handle show
]

{ #category : #cursor }
OSSDL2WindowHandle >> showCursor: aBoolean [
	SDL2 showCursor: (aBoolean ifTrue: [SDL_ENABLE] ifFalse: [SDL_DISABLE]).
]

{ #category : #'text input' }
OSSDL2WindowHandle >> startTextInput [
	"See https://wiki.libsdl.org/SDL_StartTextInput"

	handle startTextInput
]

{ #category : #'text input' }
OSSDL2WindowHandle >> stopTextInput [
	"See https://wiki.libsdl.org/SDL_StopTextInput"

	handle stopTextInput
]

{ #category : #accessing }
OSSDL2WindowHandle >> title [
	^ handle title
]

{ #category : #accessing }
OSSDL2WindowHandle >> title: aTitle [
	handle title: aTitle
]

{ #category : #'window management' }
OSSDL2WindowHandle >> toggleBorderOff [

	handle toggleBorder: false.
]

{ #category : #'window management' }
OSSDL2WindowHandle >> toggleBorderOn [

	handle toggleBorder: true.
]

{ #category : #'event handling' }
OSSDL2WindowHandle >> visitKeyDownEvent: event [
	| osEvent keysym |
	keysym := event keysym.
	osEvent := OSKeyDownEvent for: osWindow.
	osEvent scanCode: keysym scancode;
			symbol: keysym sym;
			repeat: event repeat;
			position: self mousePosition;
			character: (self mapSpecialCharacter: keysym sym).
			
	self convertButtonState: SDL2 mouseState modState: keysym mod modifiers: osEvent modifiers.
	^ osEvent deliver.
]

{ #category : #'event handling' }
OSSDL2WindowHandle >> visitKeyUpEvent: event [
	| osEvent keysym |
	keysym := event keysym.
	osEvent := OSKeyUpEvent for: osWindow.
	osEvent scanCode: keysym scancode;
			symbol: keysym sym;
			repeat: event repeat;
			position: self mousePosition.
			
	self convertButtonState: SDL2 mouseState modState: keysym mod modifiers: osEvent modifiers.
	^ osEvent deliver.
]

{ #category : #'event handling' }
OSSDL2WindowHandle >> visitMouseButtonDownEvent: event [
	| osEvent |
	osEvent := OSMouseButtonPressEvent for: osWindow.
	osEvent button: event button;
			position: event x @ event y.
	
	self convertButtonState: SDL2 mouseState modState: SDL2 modState modifiers: osEvent modifiers.
	^ osEvent deliver.
]

{ #category : #'event handling' }
OSSDL2WindowHandle >> visitMouseButtonUpEvent: event [
	| osEvent |
	osEvent := OSMouseButtonReleaseEvent for: osWindow.
	osEvent button: event button;
			position: event x @ event y.
	
	self convertButtonState: SDL2 mouseState modState: SDL2 modState modifiers: osEvent modifiers.
	^ osEvent deliver.
]

{ #category : #'event handling' }
OSSDL2WindowHandle >> visitMouseMotionEvent: sdlEvent [
	| osEvent |
	osEvent := OSMouseMoveEvent for: osWindow.
	osEvent position: sdlEvent x @ sdlEvent y;
		delta: sdlEvent xrel @ sdlEvent yrel.

	self convertButtonState: sdlEvent state modState: SDL2 modState modifiers: osEvent modifiers.
	^ osEvent deliver.
]

{ #category : #'event handling' }
OSSDL2WindowHandle >> visitMouseWheelEvent: sdlEvent [
	| osEvent |
	osEvent := OSMouseWheelEvent for: osWindow.
	osEvent position: self mousePosition;
			scrollHorizontal: sdlEvent x;
			scrollVertical: sdlEvent y.
			
	self convertButtonState: SDL2 mouseState modState: SDL2 modState modifiers: osEvent modifiers.
	^ osEvent deliver.
]

{ #category : #'event handling' }
OSSDL2WindowHandle >> visitTextInputEvent: event [
	| osEvent |
	
	osEvent := OSTextInputEvent for: osWindow.
	osEvent text: (ZnUTF8Encoder new decodeBytes: event text);
			position: self mousePosition.
			
	self convertButtonState: SDL2 mouseState modState: SDL2 modState modifiers: osEvent modifiers.
	^ osEvent deliver.
]

{ #category : #'event handling' }
OSSDL2WindowHandle >> visitWindowEvent: windowEvent [
	osWindow ifNil: [ ^self ].
	
	windowEvent event = SDL_WINDOWEVENT_EXPOSED ifTrue: [
		^ ((OSWindowExposeEvent for: osWindow) timestamp: windowEvent timestamp) deliver ].
	windowEvent event = SDL_WINDOWEVENT_HIDDEN ifTrue: [ 
		^ (OSWindowHiddenEvent for: osWindow) deliver ].
	windowEvent event = SDL_WINDOWEVENT_SHOWN ifTrue: [ 
		^ ((OSWindowShownEvent for: osWindow) timestamp: windowEvent timestamp) deliver ].
	windowEvent event = SDL_WINDOWEVENT_CLOSE ifTrue: [ 
		^ (OSWindowCloseEvent for: osWindow) deliver ].
	windowEvent event = SDL_WINDOWEVENT_MOVED ifTrue: [ 
		^ (OSWindowMoveEvent for: osWindow) deliver ].
	windowEvent event = SDL_WINDOWEVENT_RESIZED ifTrue: [ 
		^ (OSWindowResizeEvent for: osWindow)
				width: windowEvent data1;
				height: windowEvent data2;
				deliver ].
	windowEvent event = SDL_WINDOWEVENT_ENTER ifTrue: [ 
		^ (OSWindowMouseEnterEvent for: osWindow) deliver ].
	windowEvent event = SDL_WINDOWEVENT_LEAVE ifTrue: [ 
		^ (OSWindowMouseLeaveEvent for: osWindow) deliver ].
	windowEvent event = SDL_WINDOWEVENT_FOCUS_GAINED ifTrue: [ 
		self restoreKeyboardFocus.
		^ (OSWindowFocusInEvent for: osWindow) deliver ].
	windowEvent event = SDL_WINDOWEVENT_FOCUS_LOST ifTrue: [ 
		self resetKeyboardFocus.		
		^ (OSWindowFocusOutEvent for: osWindow) deliver ].
	windowEvent event = SDL_WINDOWEVENT_MAXIMIZED ifTrue: [ 
		^ (OSWindowMaximizedEvent for: osWindow) deliver ].
	windowEvent event = SDL_WINDOWEVENT_MINIMIZED ifTrue: [ 
		^ (OSWindowMinimizedEvent for: osWindow) deliver ]
]

{ #category : #accessing }
OSSDL2WindowHandle >> windowId [
	^ self handle windowID
]

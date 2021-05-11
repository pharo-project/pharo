"
I am the OSWindowHandle interface implemented using SDL2 library

my handle is an SDL_Window
"
Class {
	#name : #OSSDL2BackendWindow,
	#superclass : #OSBackendWindow,
	#instVars : [
		'currentCursor',
		'iconSurface',
		'sdl2Window',
		'screenScaleFactor',
		'horizontalDPI',
		'verticalDPI',
		'diagonalDPI'
	],
	#pools : [
		'SDL2Constants'
	],
	#category : #'OSWindow-SDL2-Base'
}

{ #category : #'instance creation' }
OSSDL2BackendWindow class >> newWithHandle: handle attributes: attributes [
	^ self basicNew 
		initWithHandle: handle attributes: attributes; 
		yourself
]

{ #category : #accessing }
OSSDL2BackendWindow >> borderless [
	^ sdl2Window getFlags anyMask: SDL_WINDOW_BORDERLESS
]

{ #category : #accessing }
OSSDL2BackendWindow >> borderless: aBoolean [
	sdl2Window toggleBorder: aBoolean not
]

{ #category : #accessing }
OSSDL2BackendWindow >> bounds: newBounds [
	self 
		position: newBounds origin;
		extent: newBounds extent
]

{ #category : #'mouse capture' }
OSSDL2BackendWindow >> captureMouse [
	SDL2 setRelativeMouseMode: true
]

{ #category : #accessing }
OSSDL2BackendWindow >> clipboardText [
	^ SDL2 clipboardText
]

{ #category : #accessing }
OSSDL2BackendWindow >> clipboardText: aText [
	SDL2 clipboardText: aText
]

{ #category : #'event handling' }
OSSDL2BackendWindow >> convertButtonState: mouseState modState: modState modifiers: modifiers [
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

	modifiers leftShift: shift; rightShift: shift;
		leftCtrl: ctrl; rightCtrl: ctrl;
		leftAlt: alt; rightAlt: ralt;
		leftCmd: gui; rightCmd: gui
]

{ #category : #private }
OSSDL2BackendWindow >> convertCursor: aCursor [
	| bits size result bytesPerLineInResult bytesPerLineInBitMap lineBitmapPosition lineResultPosition |

	"This method converts the bits in a Pharo Cursor to the bitmap expected by SDL.
	SDL expects a bitmap of 1bit depth. The cursor passed as parameter should be 1 bit depth.	
		
	This conversion handles cursors of any size. SDL requires that the resulting width is multiple of 8.
	"
	aCursor unhibernate.
	
	bits := aCursor bits.
	size := aCursor extent.

	"The resulting byte array has the size of the cursor (e.g., 32x32), but as each pixel is one byte 
	we need to divide it by 8"
	result := ByteArray new: (size x * size y) // 8.

	"We are going to traverse the whole Pharo bitmap and generate the byteArray.
	As the bitmap is organized line by line, we need to calculate how many bytes there is in a line."
	bytesPerLineInResult := size x // 8.
	"The amount of bytes in a line in the bitmap is not the same as the result.
	Bitmaps contain 32-bit numbers. So each line is padded to 32 bits elements (4 bytes). 
	If the line has 48 pixels, in the byteArray it will take 6 bytes, but in the bitmap this is round up to 8 bytes.
	The padding is always zero and we can discard it in the transformation (as it has not image information)"
	bytesPerLineInBitMap := bytesPerLineInResult roundUpTo: 4.

	"We are going to traverse the cursor line by line.
	As the positions in the bitmap and the byte array are not the same (the bitmap has padding, the bytearray no)
	we need to have different starts for each line.
	Once the offset is adjusted we can copy byte-per-byte"	
	1 to: size y do: [ :line |
		lineBitmapPosition := (line - 1) * bytesPerLineInBitMap.
		lineResultPosition := (line - 1) * bytesPerLineInResult.
		
		1 to: bytesPerLineInResult do: [ :i |
			result at: lineResultPosition + i put: (bits byteAt: lineBitmapPosition + i)]].
	

	^ result 
	
]

{ #category : #cursor }
OSSDL2BackendWindow >> convertCursor: cursor withMask: mask andScale: scale [

	| maskBits extent cursorBits offset sdlCursor scaledCursor scaledMask scaleToUse |

	scaleToUse := scale ceiling.

	^ self 
		lookupSystemCursor: cursor 
		ifNotFound: [ 
			scaledCursor := cursor scaledToSize: (cursor extent * scaleToUse).
			scaledMask := mask scaledToSize: (mask extent * scaleToUse).

			cursorBits := self convertCursor: scaledCursor.
			maskBits := self convertCursor: scaledMask.
			extent := scaledCursor extent.
			offset := scaledCursor offset.
			sdlCursor := SDL2
				             createCursor: cursorBits
				             mask: maskBits
				             w: extent x
				             h: extent y
				             hotX: offset x negated
				             hotY: offset y negated ].
	
]

{ #category : #'event handling' }
OSSDL2BackendWindow >> deliverGlobalEvent: aGlobalEvent [
	^ osWindow deliverGlobalEvent: aGlobalEvent
]

{ #category : #deleting }
OSSDL2BackendWindow >> destroy [

	sdl2Window ifNotNil: [ :validHandle | 
		OSSDL2Driver current unregisterWindowWithId: validHandle windowID.
		renderer ifNotNil: [ renderer destroy ].
		validHandle destroy
	].

	renderer := nil.
	sdl2Window := nil.
	osWindow := nil
]

{ #category : #accessing }
OSSDL2BackendWindow >> diagonalDPI [
	self fetchDPIAndRaiseEvent.
	^ diagonalDPI
]

{ #category : #accessing }
OSSDL2BackendWindow >> extent [
	| w h |
	w := ByteArray new: 4.
	h := ByteArray new: 4.
	sdl2Window getSizeW: w h: h.
	^ (w signedLongAt: 1) @ (h signedLongAt: 1)
]

{ #category : #accessing }
OSSDL2BackendWindow >> extent: newExtent [
	sdl2Window setSizeW: newExtent x h: newExtent y
]

{ #category : #private }
OSSDL2BackendWindow >> fetchDPI [
	| displayIndex ddpi hdpi vdpi newDiagonalDPI newScreenScaleFactor |
	"In OS X disable the computation of a DPI based scale factor. These values
	are completely wrong and we are getting scale factors that are very large
	even in non-retina display. This workaround does not affect the support for
	retina display because that is handled by fetching the size of the drawing surface."
	Smalltalk os isMacOSX ifTrue: [ 
		diagonalDPI := verticalDPI := horizontalDPI := self screenScaleFactorBaseDPI.
		screenScaleFactor := 1.0.
		^ false
	].

	displayIndex := sdl2Window getDisplayIndex.
	displayIndex < 0 ifTrue: [ ^ false ].
	
	ddpi := ByteArray new: 4.
	hdpi := ByteArray new: 4.
	vdpi := ByteArray new: 4.
	(SDL2 getDisplay: displayIndex ddpi: ddpi hdpi: hdpi vdpi: vdpi) = 0 ifFalse: [ ^ false ].
	
	newDiagonalDPI := ddpi floatAt: 1.
	"Allow automatic scale of factor increments of 5%. We need to
	round due to precision issues which may result in an almost 1 scale factor, but not exactly one.
	This has to be at least a divisor of 25% which are normal scale factor increments that are selectable in Windows.
	"
	newScreenScaleFactor := newDiagonalDPI / self screenScaleFactorBaseDPI roundTo: 0.05.
	(screenScaleFactor closeTo: newScreenScaleFactor) ifTrue: [ ^ false ].

	horizontalDPI := hdpi floatAt: 1.
	verticalDPI := vdpi floatAt: 1.
	diagonalDPI := newDiagonalDPI.
	screenScaleFactor := newScreenScaleFactor.
	^ true
]

{ #category : #private }
OSSDL2BackendWindow >> fetchDPIAndRaiseEvent [
	self fetchDPI ifTrue: [
		(OSWindowResolutionChangeEvent for: osWindow)
			horizontalDPI: horizontalDPI;
			verticalDPI: verticalDPI;
			diagonalDPI: diagonalDPI;
			screenScaleFactor: screenScaleFactor;
			deliver
	]
]

{ #category : #accessing }
OSSDL2BackendWindow >> fullscreen: aBoolean [

	aBoolean 
		ifTrue: [ sdl2Window fullscreen: SDL_WINDOW_FULLSCREEN_DESKTOP ] 
		ifFalse: [ sdl2Window fullscreen: 0 ]
]

{ #category : #private }
OSSDL2BackendWindow >> getFlags [
	^ sdl2Window getFlags
]

{ #category : #private }
OSSDL2BackendWindow >> getWMInfo [
	| wmInfo |
	wmInfo := SDL_SysWMinfo new version: SDL_Version bindingVersion.
	sdl2Window getWMInfo: wmInfo.
	^ wmInfo
]

{ #category : #'event handling' }
OSSDL2BackendWindow >> handleNewSDLEvent: sdlEvent [
	^ sdlEvent accept: self
]

{ #category : #accessing }
OSSDL2BackendWindow >> hide [
	sdl2Window hide
]

{ #category : #accessing }
OSSDL2BackendWindow >> horizontalDPI [
	self fetchDPIAndRaiseEvent.
	^ horizontalDPI
]

{ #category : #accessing }
OSSDL2BackendWindow >> icon: aForm [
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
	sdl2Window icon: surface.
	iconSurface ifNotNil: [ SDL2 freeSurface: surface ].
	iconSurface := surface
]

{ #category : #'private - initialization' }
OSSDL2BackendWindow >> initWithHandle: aHandle attributes: attributes [
	sdl2Window := aHandle.
	attributes applyTo: self.
	diagonalDPI := horizontalDPI := verticalDPI := self screenScaleFactorBaseDPI.
	screenScaleFactor := 1.
	self fetchDPI.
]

{ #category : #initialization }
OSSDL2BackendWindow >> initialize [
	super initialize.
	self class initializeSlots: self

]

{ #category : #'text input' }
OSSDL2BackendWindow >> isTextInputActive [
	"See https://wiki.libsdl.org/SDL_IsTextInputActive"

	^ sdl2Window isTextInputActive
]

{ #category : #testing }
OSSDL2BackendWindow >> isValid [
	^ true
]

{ #category : #testing }
OSSDL2BackendWindow >> isVisible [
	^ (self getFlags bitAnd: SDL_WINDOW_SHOWN) = SDL_WINDOW_SHOWN
]

{ #category : #cursor }
OSSDL2BackendWindow >> lookupSystemCursor: cursor ifNotFound: aBlock [
	
	^ self systemCursorConversionTable
		  detect: [ :aMapping | (Cursor perform: aMapping key) = cursor ]
		  ifFound: [ :aMapping | SDL2 createSystemCursor: (SDL_SystemCursorType perform: aMapping value) ]
		  ifNone: [ aBlock value  ]
]

{ #category : #'event handling' }
OSSDL2BackendWindow >> mapSpecialCharacter: symbol [
	^ SDL2SpecialCharacterMapping mapKeySymbol: symbol ifAbsent: [ ^ nil ]
]

{ #category : #'window management' }
OSSDL2BackendWindow >> maximize [

	sdl2Window maximize
]

{ #category : #'window management' }
OSSDL2BackendWindow >> minimize [

	sdl2Window minimize
]

{ #category : #'event handling' }
OSSDL2BackendWindow >> mousePosition [
	| x y |
	x := ByteArray new: 4.
	y := ByteArray new: 4.
	SDL2 mouseStateX: x y: y.
	^ ( x signedLongAt: 1) @ (y signedLongAt: 1)
]

{ #category : #'instance creation' }
OSSDL2BackendWindow >> newAthensRenderer [
	^ renderer := OSSDL2AthensRenderer for: self
]

{ #category : #'instance creation' }
OSSDL2BackendWindow >> newFormRenderer: form [
	^ renderer := OSSDL2FormRenderer new 
		form: form;
		backendWindow: self;
		yourself
]

{ #category : #'instance creation' }
OSSDL2BackendWindow >> newGenericRenderer [
	^ renderer := OSSDL2GenericRenderer new 
		backendWindow: self;
		createSDL2Renderer;
		yourself
]

{ #category : #'instance creation' }
OSSDL2BackendWindow >> newOpenGLRenderer [
	^ renderer := OSSDL2GLRenderer new 
		backendWindow: self;
		createContext;
		yourself
]

{ #category : #accessing }
OSSDL2BackendWindow >> platformSpecificHandle [
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
OSSDL2BackendWindow >> position [
	| x y |
	x := ByteArray new: ExternalType long byteSize.
	y := ByteArray new: ExternalType long byteSize.
	sdl2Window getPositionX: x y: y.
	^ ( x signedLongAt: 1) @ (y signedLongAt: 1)
]

{ #category : #accessing }
OSSDL2BackendWindow >> position: aPoint [
	sdl2Window setPositionX: aPoint x asInteger y: aPoint y asInteger
]

{ #category : #accessing }
OSSDL2BackendWindow >> prepareExternalResourceForAutoRelease [
	sdl2Window autoRelease
]

{ #category : #accessing }
OSSDL2BackendWindow >> raise [

	sdl2Window raise
]

{ #category : #'mouse capture' }
OSSDL2BackendWindow >> releaseMouse [
	SDL2 setRelativeMouseMode: false
]

{ #category : #accessing }
OSSDL2BackendWindow >> resizable [
	^ sdl2Window getFlags anyMask: SDL_WINDOW_RESIZABLE
]

{ #category : #accessing }
OSSDL2BackendWindow >> resizable: aBoolean [
	"This is not supported."
]

{ #category : #'window management' }
OSSDL2BackendWindow >> restore [

	sdl2Window restore
]

{ #category : #accessing }
OSSDL2BackendWindow >> screenScaleFactor [
	self fetchDPIAndRaiseEvent.
	^ screenScaleFactor
]

{ #category : #accessing }
OSSDL2BackendWindow >> sdl2Window [

	^ sdl2Window
]

{ #category : #'window management' }
OSSDL2BackendWindow >> setDraggableArea: aRectangle [

	| myCallback sdlRect |
	
	sdlRect := aRectangle asSDLRect.
	
	myCallback := DraggableCallback on: [ :aWindow :pt :data |
			(pt x > sdlRect x) & (pt x < sdlRect w) & (pt y > sdlRect y) & (pt y < sdlRect h) ifTrue: [ 1 ] ifFalse: [ 0 ].
		].
	
	^sdl2Window setHitTest: myCallback
]

{ #category : #cursor }
OSSDL2BackendWindow >> setMouseCursor: cursor mask: mask andScale: scale [

	| sdlCursor |
	sdlCursor := self convertCursor: cursor withMask: mask andScale: scale.

	sdlCursor ifNotNil: [ sdlCursor setCursor ].
	currentCursor ifNotNil: [ currentCursor freeCursor ].
	currentCursor := sdlCursor
]

{ #category : #accessing }
OSSDL2BackendWindow >> show [
	sdl2Window show
]

{ #category : #cursor }
OSSDL2BackendWindow >> showCursor: aBoolean [
	SDL2 showCursor: (aBoolean ifTrue: [SDL_ENABLE] ifFalse: [SDL_DISABLE])
]

{ #category : #'text input' }
OSSDL2BackendWindow >> startTextInput [
	"See https://wiki.libsdl.org/SDL_StartTextInput"

	sdl2Window startTextInput
]

{ #category : #'text input' }
OSSDL2BackendWindow >> stopTextInput [
	"See https://wiki.libsdl.org/SDL_StopTextInput"

	sdl2Window stopTextInput
]

{ #category : #cursor }
OSSDL2BackendWindow >> systemCursorConversionTable [

	^ OSPlatform current sdlPlatform systemCursorConversionTable
]

{ #category : #accessing }
OSSDL2BackendWindow >> title [
	^ sdl2Window title
]

{ #category : #accessing }
OSSDL2BackendWindow >> title: aTitle [
	sdl2Window title: aTitle
]

{ #category : #'window management' }
OSSDL2BackendWindow >> toggleBorderOff [

	sdl2Window toggleBorder: false
]

{ #category : #'window management' }
OSSDL2BackendWindow >> toggleBorderOn [

	sdl2Window toggleBorder: true
]

{ #category : #accessing }
OSSDL2BackendWindow >> verticalDPI [
	self fetchDPIAndRaiseEvent.
	^ verticalDPI
]

{ #category : #'event handling' }
OSSDL2BackendWindow >> visitKeyDownEvent: event [
	| osEvent keysym |
	keysym := event keysym.
	osEvent := OSKeyDownEvent for: osWindow.
	osEvent scanCode: keysym scancode;
			symbol: keysym sym;
			repeat: event repeat;
			position: self mousePosition;
			character: (self mapSpecialCharacter: keysym sym).
			
	self convertButtonState: SDL2 mouseState modState: keysym mod modifiers: osEvent modifiers.
	^ osEvent deliver
]

{ #category : #'event handling' }
OSSDL2BackendWindow >> visitKeyUpEvent: event [
	| osEvent keysym |
	keysym := event keysym.
	osEvent := OSKeyUpEvent for: osWindow.
	osEvent scanCode: keysym scancode;
			symbol: keysym sym;
			repeat: event repeat;
			position: self mousePosition.
			
	self convertButtonState: SDL2 mouseState modState: keysym mod modifiers: osEvent modifiers.
	^ osEvent deliver
]

{ #category : #'event handling' }
OSSDL2BackendWindow >> visitMouseButtonDownEvent: event [
	| osEvent |
	osEvent := OSMouseButtonPressEvent for: osWindow.
	osEvent button: event button;
			position: event x @ event y.
	
	self convertButtonState: SDL2 mouseState modState: SDL2 modState modifiers: osEvent modifiers.
	^ osEvent deliver
]

{ #category : #'event handling' }
OSSDL2BackendWindow >> visitMouseButtonUpEvent: event [
	| osEvent |
	osEvent := OSMouseButtonReleaseEvent for: osWindow.
	osEvent button: event button;
			position: event x @ event y.
	
	self convertButtonState: SDL2 mouseState modState: SDL2 modState modifiers: osEvent modifiers.
	^ osEvent deliver
]

{ #category : #'event handling' }
OSSDL2BackendWindow >> visitMouseMotionEvent: sdlEvent [
	| osEvent |
	osEvent := OSMouseMoveEvent for: osWindow.
	osEvent position: sdlEvent x @ sdlEvent y;
		delta: sdlEvent xrel @ sdlEvent yrel.

	self convertButtonState: sdlEvent state modState: SDL2 modState modifiers: osEvent modifiers.
	^ osEvent deliver
]

{ #category : #'event handling' }
OSSDL2BackendWindow >> visitMouseWheelEvent: sdlEvent [
	| osEvent |
	osEvent := OSMouseWheelEvent for: osWindow.
	osEvent position: self mousePosition;
			scrollHorizontal: sdlEvent x;
			scrollVertical: sdlEvent y.
			
	self convertButtonState: SDL2 mouseState modState: SDL2 modState modifiers: osEvent modifiers.
	^ osEvent deliver
]

{ #category : #'event handling' }
OSSDL2BackendWindow >> visitTextInputEvent: event [
	| osEvent |
	
	osEvent := OSTextInputEvent for: osWindow.
	osEvent text: (ZnUTF8Encoder new decodeBytes: event text);
			position: self mousePosition.
	^ osEvent deliver
]

{ #category : #'event handling' }
OSSDL2BackendWindow >> visitWindowEvent: windowEvent [
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
		self fetchDPIAndRaiseEvent.
		^ (OSWindowMoveEvent for: osWindow) deliver
	].
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
		^ (OSWindowFocusInEvent for: osWindow) deliver ].
	windowEvent event = SDL_WINDOWEVENT_FOCUS_LOST ifTrue: [ 
		^ (OSWindowFocusOutEvent for: osWindow) deliver ].
	windowEvent event = SDL_WINDOWEVENT_MAXIMIZED ifTrue: [ 
		^ (OSWindowMaximizedEvent for: osWindow) deliver ].
	windowEvent event = SDL_WINDOWEVENT_MINIMIZED ifTrue: [ 
		^ (OSWindowMinimizedEvent for: osWindow) deliver ]
]

{ #category : #accessing }
OSSDL2BackendWindow >> windowId [
	^ self sdl2Window windowID
]

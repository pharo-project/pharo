"
A window driver used for running things using SDL2 library
"
Class {
	#name : #OSSDL2Driver,
	#superclass : #OSWindowDriver,
	#instVars : [
		'inputSemaphore',
		'eventQueue',
		'globalListeners'
	],
	#classVars : [
		'EventLoopProcess',
		'JoystickMap',
		'WindowMap'
	],
	#pools : [
		'SDL2Constants'
	],
	#category : 'OSWindow-SDL2'
}

{ #category : #testing }
OSSDL2Driver class >> hasPlugin [
	<primitive: #primitiveHasDisplayPlugin module: #SDL2DisplayPlugin>
	^ false
]

{ #category : #testing }
OSSDL2Driver class >> isSuitable [
	^ self isSupported
]

{ #category : #testing }
OSSDL2Driver class >> isSupported [ 
	^ SDL2 isAvailable
]

{ #category : #testing }
OSSDL2Driver class >> isVMDisplayUsingSDL2 [
	<primitive: #primitiveIsVMDisplayUsingSDL2>
	^ false
]

{ #category : #private }
OSSDL2Driver >> closeRemovedController: index [
	"TODO: What should I do here?"
	Transcript show: 'Game controller '; show: index; show: 'removed'; cr.
]

{ #category : #private }
OSSDL2Driver >> closeRemovedJoystick: index [
	"TODO: What should I do here?"
	Transcript show: 'Joystick '; show: index; show: 'removed'; cr.
]

{ #category : #events-processing }
OSSDL2Driver >> convertEvent: sdlEvent [
	| mappedEvent window |
	
	mappedEvent := sdlEvent mapped ifNil: [ ^ nil ].
	mappedEvent windowID ifNil: [ ^ self sendEventWithoutWindow: mappedEvent ].
	
	window := WindowMap at: mappedEvent windowID ifAbsent: [ ^ nil ].
	window handleNewSDLEvent: mappedEvent.
	^ nil
	
]

{ #category : #'window creation and deletion' }
OSSDL2Driver >> createWindowWithAttributes: attributes osWindow: osWindow [
	| flags handle window glAttributes x y |
	
	flags := attributes visible ifTrue: [ SDL_WINDOW_SHOWN ] ifFalse: [ SDL_WINDOW_HIDDEN ].
	attributes fullscreen ifTrue: [ flags := flags | SDL_WINDOW_FULLSCREEN_DESKTOP ].
	attributes borderless ifTrue: [ flags := flags | SDL_WINDOW_BORDERLESS ].
	flags := flags | SDL_WINDOW_ALLOW_HIGHDPI.
	
	" Set the OpenGL attributes."
	glAttributes := attributes glAttributes.
	glAttributes ifNotNil: [ 
		flags := flags | SDL_WINDOW_OPENGL.
		self setGLAttributes: glAttributes ].
	
	"Get the actual initial position value."
	x := attributes x.
	y := attributes y.
	attributes windowCentered ifTrue: [
		x := SDL_WINDOWPOS_CENTERED.
		y := SDL_WINDOWPOS_CENTERED ].

	"Extra creation flags"
	attributes resizable ifTrue: [ 
		flags := flags | SDL_WINDOW_RESIZABLE ].

	attributes maximized ifTrue: [ 
		flags := flags | SDL_WINDOW_MAXIMIZED ].
	
	attributes minimized ifTrue: [ 
		flags := flags | SDL_WINDOW_MINIMIZED ].

	"Create the window"
	handle := SDL2 createWindow: attributes title
		x: x
		y: y
		width: attributes width 
		height: attributes height 
		flags: flags.
	
	window := OSSDL2WindowHandle newWithHandle: handle attributes: attributes.
	window osWindow: osWindow.
	self registerWindow: window.
	^ window
]

{ #category : #events-processing }
OSSDL2Driver >> eventLoopProcessWithPlugin [
	| event session semaIndex |
	event := SDL_Event new.
	session := Smalltalk session.
	
	semaIndex := (Smalltalk registerExternalObject: inputSemaphore).
	self primSetupInputSemaphore: semaIndex.
	[
		[ inputSemaphore wait; consumeAllSignals. session  == Smalltalk session] whileTrue: [
			[ (SDL2 pollEvent: event) > 0 ] whileTrue: [
				self processEvent: event
			].
		]
	] ensure:  [ Smalltalk unregisterExternalObject: inputSemaphore ]
]

{ #category : #events-processing }
OSSDL2Driver >> eventLoopProcessWithVMWindow [
	| event session semaIndex eventBuffer |
	event := SDL_Event new.
	eventBuffer := event getHandle. 
	session := Smalltalk session.
	
	semaIndex := (Smalltalk registerExternalObject: inputSemaphore).
	self primitiveSetVMSDL2Input: semaIndex.
	[
		[ inputSemaphore wait; consumeAllSignals. session  == Smalltalk session] whileTrue: [
			[ self primitivePollVMSDL2Event: eventBuffer size: eventBuffer size ] whileTrue: [
				self processEvent: event
			].
		]
	] ensure:  [ Smalltalk unregisterExternalObject: inputSemaphore ]
]

{ #category : #events-processing }
OSSDL2Driver >> eventLoopProcessWithoutPlugin [
	| event session |
	event := SDL_Event new.
	session := Smalltalk session.
	
	[ session == Smalltalk session] 
	whileTrue: [
		[ (SDL2 pollEvent: event) > 0 ] 
		whileTrue: [ self processEvent: event ].
		
		(Delay forMilliseconds: 5) wait ]
]

{ #category : #initialization }
OSSDL2Driver >> initialize [
	inputSemaphore := Semaphore new.
	eventQueue := AtomicSharedQueue new.
	globalListeners := WeakSet new.
	self
		initializeWindowMap;
		initializeJoystickMap.
	SDL2
		initVideo;
		initJoystick;
		initGameController.
	self setupEventLoop.
]

{ #category : #'window creation and deletion' }
OSSDL2Driver >> initializeJoystickMap [

	JoystickMap ifNil: [ 
		JoystickMap := Dictionary new. 
	].
]

{ #category : #'window creation and deletion' }
OSSDL2Driver >> initializeWindowMap [

	WindowMap ifNil: [ 
		WindowMap := WeakValueDictionary new. 
	].
]

{ #category : #'global events' }
OSSDL2Driver >> isGameController: joystickIndex [
	^ (JoystickMap at: joystickIndex ifAbsent: [ ^ false ]) isGameController
]

{ #category : #private }
OSSDL2Driver >> openAddedController: index [
	| controller |
	controller := SDL2 gameControllerOpen: index.
	JoystickMap at: index put: controller.
	
]

{ #category : #private }
OSSDL2Driver >> openAddedJoystick: index [
	| joystick |
	joystick := SDL2 joystickOpen: index.
	JoystickMap at: index put: joystick.

]

{ #category : #events-processing }
OSSDL2Driver >> primSetupInputSemaphore: semaIndex [
	<primitive: #primitiveSetSDL2Input module: #SDL2DisplayPlugin>
	^ nil
]

{ #category : #events-processing }
OSSDL2Driver >> primitivePollVMSDL2Event: eventBuffer size: size [
	<primitive: #primitivePollVMSDL2Event>
	^ false
]

{ #category : #events-processing }
OSSDL2Driver >> primitiveSetVMSDL2Input: semaIndex [
	<primitive: #primitiveSetVMSDL2Input>
	^ nil
]

{ #category : #events-processing }
OSSDL2Driver >> processEvent: sdlEvent [
	| event |
	[  
		event := self convertEvent: sdlEvent.
		event ifNotNil: [ eventQueue nextPut: event ].
	] on: Error do: [ :err | 
		"It is critical, that event handling keep running despite errors.
		Normally, any errors in event handling requires immediate attention and fixing"
		err freeze.
		[ err debug ] fork.
	 ]
]

{ #category : #'global events' }
OSSDL2Driver >> registerGlobalListener: globalListener [
	"This method registers a global event listener.
	Global event listeners are used to handle events that are not generate for a specific window.
	Joysticks use this kind of events.
	"
	globalListeners add: globalListener
]

{ #category : #'window creation and deletion' }
OSSDL2Driver >> registerWindow: window [
	WindowMap at: window handle windowID put: window
]

{ #category : #'global events' }
OSSDL2Driver >> sendEventWithoutWindow: event [
	| converted |
	converted := event accept: self.
	converted ifNil: [ ^ self ].
	globalListeners do: [ :list | list handleEvent: converted ].
	WindowMap valuesDo: [ :window | window deliverGlobalEvent: converted ].
]

{ #category : #'window creation and deletion' }
OSSDL2Driver >> setGLAttributes: glAttributes [
"	SDL2 glSetAttribute: SDL_GL_CONTEXT_MAJOR_VERSION value: glAttributes majorVersion.
	SDL2 glSetAttribute: SDL_GL_CONTEXT_MINOR_VERSION value: glAttributes minorVersion.
	SDL2 glSetAttribute: SDL_GL_CONTEXT_PROFILE_MASK value: glAttributes profileType.
"


	SDL2 glSetAttribute: SDL_GL_RED_SIZE value: glAttributes redSize.
	SDL2 glSetAttribute: SDL_GL_BLUE_SIZE value: glAttributes blueSize.
	SDL2 glSetAttribute: SDL_GL_GREEN_SIZE value: glAttributes greenSize.
	SDL2 glSetAttribute: SDL_GL_ALPHA_SIZE value: glAttributes alphaSize.
	SDL2 glSetAttribute: SDL_GL_DEPTH_SIZE value: glAttributes depthSize.
	SDL2 glSetAttribute: SDL_GL_STENCIL_SIZE value: glAttributes stencilSize.
	SDL2 glSetAttribute: SDL_GL_DOUBLEBUFFER value: glAttributes doubleBuffer.

	
]

{ #category : #events-processing }
OSSDL2Driver >> setupEventLoop [
	EventLoopProcess ifNotNil: [ EventLoopProcess terminate ].
	EventLoopProcess := self class isVMDisplayUsingSDL2 ifTrue: [ 
		[ self eventLoopProcessWithVMWindow ] 
		forkAt: Processor lowIOPriority
	] ifFalse: [ 
		self class hasPlugin ifTrue: [ 
			[ self eventLoopProcessWithPlugin ] 
			forkAt: Processor lowIOPriority ]
		ifFalse: [ 
			[ self eventLoopProcessWithoutPlugin ] 
			forkAt: Processor lowIOPriority ].
	].

	EventLoopProcess
		name: 'SDL2 Event loop';
		resume
]

{ #category : #'global events' }
OSSDL2Driver >> unregisterGlobalListener: globalListener [
	"This method registers a global event listener.
	Global event listeners are used to handle events that are not generate for a specific window.
	Joysticks use this kind of events.
	"
	globalListeners remove: globalListener
]

{ #category : #'window creation and deletion' }
OSSDL2Driver >> unregisterWindowWithId: windowId [

	WindowMap removeKey: windowId.
	
	"Does it make sense to keep the SDL event loop process running even if no OSWindow is currently open ?"
	"WindowMap ifEmpty: [ 
		EventLoopProcess terminate.
		EventLoopProcess := nil.
	]."
]

{ #category : #'global events' }
OSSDL2Driver >> visitCommonEvent: event [
	^ nil
]

{ #category : #'global events' }
OSSDL2Driver >> visitControllerAxisEvent: joyEvent [
	^ OSJoyAxisEvent new
		which: joyEvent which;
		axis: joyEvent axis;
		mapped: true;
		hasMapping: true;
		value: joyEvent value
]

{ #category : #'global events' }
OSSDL2Driver >> visitControllerButtonDownEvent: joyEvent [
	^ OSJoyButtonDownEvent new
		which: joyEvent which;
		button: joyEvent button;
		mapped: true;
		hasMapping: true;
		pressed: true
]

{ #category : #'global events' }
OSSDL2Driver >> visitControllerButtonUpEvent: joyEvent [
	^ OSJoyButtonUpEvent new
		which: joyEvent which;
		button: joyEvent button;
		mapped: true;
		hasMapping: true;
		pressed: false
]

{ #category : #'global events' }
OSSDL2Driver >> visitControllerDeviceAddedEvent: controllerEvent [
	self openAddedController: controllerEvent which.
	^ OSJoyDeviceAddedEvent new
		which: controllerEvent which;
		mapped: true;
		hasMapping: true
]

{ #category : #'global events' }
OSSDL2Driver >> visitControllerDeviceRemappedEvent: controllerEvent [
	^ OSJoyDeviceRemappedEvent new
		which: controllerEvent which;
		mapped: true;
		hasMapping: true
]

{ #category : #'global events' }
OSSDL2Driver >> visitControllerDeviceRemovedEvent: controllerEvent [
	self closeRemovedController: controllerEvent which.
	^ OSJoyDeviceRemovedEvent new
		which: controllerEvent which;
		mapped: true;
		hasMapping: true
]

{ #category : #'global events' }
OSSDL2Driver >> visitDropEvent: dropEvent [
	^ OSWindowDropEvent new
		timestamp: dropEvent timestamp;
		filePath: dropEvent file readString
]

{ #category : #events-processing }
OSSDL2Driver >> visitFingerDownEvent: event [
	^OSTouchActionPointerDownEvent new
			timestamp: event timestamp;
			deviceId: event touchId;
			fingerId: event fingerId;
			position: event x @ event y;
			delta: event dx @ event dy;
			pressure: event pressure;
			yourself.
	
]

{ #category : #events-processing }
OSSDL2Driver >> visitFingerMotionEvent: event [
	^OSTouchActionPointerMoveEvent new
			timestamp: event timestamp;
			deviceId: event touchId;
			fingerId: event fingerId;
			position: event x @ event y;
			delta: event dx @ event dy;
			pressure: event pressure;
			yourself.
	
]

{ #category : #events-processing }
OSSDL2Driver >> visitFingerUpEvent: event [
	^OSTouchActionPointerUpEvent new
			timestamp: event timestamp;
			deviceId: event touchId;
			fingerId: event fingerId;
			position: event x @ event y;
			delta: event dx @ event dy;
			pressure: event pressure;
			yourself.
	
]

{ #category : #'global events' }
OSSDL2Driver >> visitJoyAxisEvent: joyEvent [
	^ OSJoyAxisEvent new
		which: joyEvent which;
		axis: joyEvent axis;
		hasMapping: (self isGameController: joyEvent which);
		mapped: false;
		value: joyEvent value
]

{ #category : #'global events' }
OSSDL2Driver >> visitJoyButtonDownEvent: joyEvent [
	^ OSJoyButtonDownEvent new
		which: joyEvent which;
		button: joyEvent button;
		hasMapping: (self isGameController: joyEvent which);
		mapped: false;
		pressed: true
]

{ #category : #'global events' }
OSSDL2Driver >> visitJoyButtonUpEvent: joyEvent [
	^ OSJoyButtonUpEvent new
		which: joyEvent which;
		button: joyEvent button;
		hasMapping: (self isGameController: joyEvent which);
		mapped: false;
		pressed: false
]

{ #category : #'global events' }
OSSDL2Driver >> visitJoyDeviceAddedEvent: joyEvent [
	(SDL2 isGameController: joyEvent which) ifFalse: [ self openAddedJoystick: joyEvent which ].
	^ OSJoyDeviceAddedEvent new
		which: joyEvent which;
		hasMapping: (self isGameController: joyEvent which);
		mapped: false
]

{ #category : #'global events' }
OSSDL2Driver >> visitJoyDeviceRemovedEvent: joyEvent [
	(SDL2 isGameController: joyEvent which) ifFalse: [ self closeRemovedJoystick: joyEvent which ].
	^ OSJoyDeviceRemovedEvent new
		which: joyEvent which;
		hasMapping: (self isGameController: joyEvent which);
		mapped: false
]

{ #category : #'global events' }
OSSDL2Driver >> visitQuitEvent: quitEvent [
	^ nil
]

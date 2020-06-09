Class {
	#name : #OSSDL2DriverTest,
	#superclass : #TestCase,
	#instVars : [
		'mockedClasses'
	],
	#category : #'OSWindow-SDL2-Tests'
}

{ #category : #'as yet unclassified' }
OSSDL2DriverTest >> mock: anOSSDL2BackendWindowForTesting [ 
	
	self mockedClasses add: anOSSDL2BackendWindowForTesting class.
	anOSSDL2BackendWindowForTesting class methods do: [ :cm |
		| link |
		link := MetaLink new
			metaObject: self;
			selector: #message:withArguments:receivedBy:;
			arguments: #(selector arguments receiver);
			control: #before;
			yourself.
		cm ast link: link ]
]

{ #category : #'as yet unclassified' }
OSSDL2DriverTest >> mockedClasses [
	^ mockedClasses ifNil: [ mockedClasses := Set new ]
]

{ #category : #'as yet unclassified' }
OSSDL2DriverTest >> tearDown [
	self mockedClasses do: [ :e | e recompile ]
]

{ #category : #tests }
OSSDL2DriverTest >> testConvertEvent [
	| sdlEvent driver window result |
	sdlEvent := SDL_Event exampleKeyDownEvent.
	driver := OSSDL2DriverForTesting basicNew.
	window := OSSDL2BackendWindowForTesting new.
	self mock: window.
	driver windowMap at: sdlEvent mapped windowID put: window.
	result := driver convertEvent: sdlEvent.
	self assert: (window receivedMessages includes: (1)).
]

{ #category : #tests }
OSSDL2DriverTest >> testConvertEventWithInvalidWindowID [
	| sdlEvent driver window result |
	sdlEvent := SDL_Event exampleKeyDownEventInvalidWindowID.
	driver := OSSDL2DriverForTesting basicNew.
	window := OSSDL2BackendWindowForTesting new.
	driver windowMap at: sdlEvent mapped windowID + 1 put: window.
	result := driver convertEvent: sdlEvent.
	self assert: result isNil.
	self assert: (window receivedMessages includes: (1)).
]

{ #category : #tests }
OSSDL2DriverTest >> testProcessEvent [
	| sdlEvent driver |
	sdlEvent := SDL_Event exampleKeyDownEvent.
	driver := OSSDL2DriverForTesting basicNew.
	driver processEvent: sdlEvent.
	self assert: false.
]

{ #category : #tests }
OSSDL2DriverTest >> testProcessEventWithInvalidWindowID [
	| sdlEvent driver |
	sdlEvent := SDL_Event exampleKeyDownEventInvalidWindowID.
	driver := OSSDL2Driver basicNew.
	(driver convertEvent: sdlEvent) inspect.
	driver processEvent: sdlEvent.
	self assert: false.
]

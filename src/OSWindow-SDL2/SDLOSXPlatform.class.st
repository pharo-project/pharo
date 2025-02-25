"
I execute specific operations on SDL for OSX
"
Class {
	#name : 'SDLOSXPlatform',
	#superclass : 'SDLAbstractPlatform',
	#category : 'OSWindow-SDL2-Platforms',
	#package : 'OSWindow-SDL2',
	#tag : 'Platforms'
}

{ #category : 'operations' }
SDLOSXPlatform >> afterMainPharoWindowCreated: aOSSDLWindow [
]

{ #category : 'operations' }
SDLOSXPlatform >> afterSetWindowTitle: aString onWindow: aOSSDLWindow [

	| aParam cocoaWindow wmInfo selector |
	aParam := ObjCLibrary uniqueInstance nsStringOf: aString.

	wmInfo := aOSSDLWindow backendWindow getWMInfo.
	cocoaWindow := wmInfo info cocoa window.

	selector := ObjCLibrary uniqueInstance lookupSelector: 'setTitleWithRepresentedFilename:'.

	ObjCLibrary uniqueInstance sendMessage: selector to: cocoaWindow getHandle with: aParam.

	ObjCLibrary uniqueInstance release: aParam
]

{ #category : 'private - initialization' }
SDLOSXPlatform >> allowTouchpadInertia [

	| nsUserDefaultsClass standardUserDefaultsSel standardUserDefaults key setBoolForKeySel |
	nsUserDefaultsClass := ObjCLibrary uniqueInstance lookupClass: 'NSUserDefaults'.

	standardUserDefaultsSel := ObjCLibrary uniqueInstance
		                           lookupSelector: 'standardUserDefaults'.

	standardUserDefaults := ObjCLibrary uniqueInstance
		                        sendMessage: standardUserDefaultsSel
		                        to: nsUserDefaultsClass.

	key := ObjCLibrary uniqueInstance nsStringOf: 'AppleMomentumScrollSupported'.

	setBoolForKeySel := ObjCLibrary uniqueInstance lookupSelector:
		                    'setBool:forKey:'.

	ObjCLibrary uniqueInstance
		sendMessage: setBoolForKeySel
		to: standardUserDefaults
		with: (ExternalAddress fromAddress: 1)
		with: key.
		
	ObjCLibrary uniqueInstance release: key.
]

{ #category : 'configuration' }
SDLOSXPlatform >> defaultRendererFlags [ 

	"SDL_RENDERER_PRESENTVSYNC is required on MacOS for certain screens to avoid rendering issues"
	^ super defaultRendererFlags | SDL_RENDERER_PRESENTVSYNC
]

{ #category : 'initialization' }
SDLOSXPlatform >> initPlatformSpecific [

	| sharedApplication |
	sharedApplication := ObjCLibrary uniqueInstance sharedApplication.
	
	ObjCLibrary uniqueInstance sendMessage: (ObjCLibrary uniqueInstance lookupSelector: 'finishLaunching') to: sharedApplication.
	
	SDLOSXPharoMenu uniqueInstance installInOSXWindow.
	
	self allowTouchpadInertia.

]

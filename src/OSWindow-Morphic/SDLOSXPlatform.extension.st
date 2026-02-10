Extension { #name : 'SDLOSXPlatform' }

{ #category : '*OSWindow-Morphic' }
SDLOSXPlatform >> afterMainPharoWindowCreated: aOSSDLWindow [
]

{ #category : '*OSWindow-Morphic' }
SDLOSXPlatform >> beforeMainPharoWindowClosed: aOSSDLWindow [
]

{ #category : '*OSWindow-Morphic' }
SDLOSXPlatform >> initPlatformSpecific [

	| sharedApplication |
	sharedApplication := ObjCLibrary uniqueInstance sharedApplication.
	
	ObjCLibrary uniqueInstance sendMessage: (ObjCLibrary uniqueInstance lookupSelector: 'finishLaunching') to: sharedApplication.
	
	SDLOSXPharoMenu uniqueInstance installInOSXWindow.
	
	self allowTouchpadInertia.

]

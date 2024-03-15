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

{ #category : 'initialization' }
SDLOSXPlatform >> initPlatformSpecific [
	| sel cls sharedApplication |

	sel := ObjCLibrary uniqueInstance lookupSelector: 'sharedApplication'.
	cls := ObjCLibrary uniqueInstance lookupClass: 'NSApplication'.
	sharedApplication := ObjCLibrary uniqueInstance sendMessage: sel to: cls.
	
	ObjCLibrary uniqueInstance sendMessage: (ObjCLibrary uniqueInstance lookupSelector: 'finishLaunching') to: sharedApplication.
	
	SDLOSXPharoMenu uniqueInstance installInOSXWindow.

]

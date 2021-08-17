"
I execute specific operations on SDL for OSX
"
Class {
	#name : #SDLOSXPlatform,
	#superclass : #SDLAbstractPlatform,
	#category : #'OSWindow-SDL2-Bindings'
}

{ #category : #operations }
SDLOSXPlatform >> afterSetWindowTitle: aString onWindow: aOSSDLWindow [

	| aParam cocoaWindow wmInfo selector |
	aParam := self nsStringOf: aString.

	wmInfo := aOSSDLWindow backendWindow getWMInfo.
	cocoaWindow := wmInfo info cocoa window.

	selector := self lookupSelector: 'setTitleWithRepresentedFilename:'.

	self sendMessage: selector to: cocoaWindow getHandle with: aParam.
	
	self release: aParam.

]

{ #category : #'ffi-calls' }
SDLOSXPlatform >> ffiLibraryName [

	^ 'libobjc.dylib'
]

{ #category : #initialization }
SDLOSXPlatform >> initPlatformSpecific [ 
	| sel cls |

	sel := self lookupSelector: 'sharedApplication'.
	cls := self lookupClass: 'NSApplication'.
	self sendMessage: sel to: cls
]

{ #category : #'utilities - objectiveC' }
SDLOSXPlatform >> lookupClass: aString [

	^ self ffiCall: #(void* objc_lookUpClass(char *aString))
]

{ #category : #'utilities - objectiveC' }
SDLOSXPlatform >> lookupSelector: aString [

	^ self ffiCall: #(void* sel_registerName(const char *aString))

]

{ #category : #'utilities - objectiveC' }
SDLOSXPlatform >> nsStringOf: aString [ 

	| class selector encoded param |
	class := self lookupClass: 'NSString'.
	selector:= self lookupSelector: 'stringWithUTF8String:'.
	
	encoded := aString utf8Encoded.
	param := ByteArray new: encoded size + 1.
	param pinInMemory.
	
	LibC memCopy: encoded to: param size: encoded size.
	param at: encoded size + 1 put: 0.
		
	^ self sendMessage: selector to: class with: param

]

{ #category : #'utilities - objectiveC' }
SDLOSXPlatform >> release: aObjCObject [ 

	| releaseSelector |
	releaseSelector:= self lookupSelector: 'release'.
	self sendMessage: releaseSelector to: aObjCObject.

]

{ #category : #'utilities - objectiveC' }
SDLOSXPlatform >> sendMessage: sel to: cls [

	^ self ffiCall: #(void* objc_msgSend(void* cls, void* sel))
]

{ #category : #'utilities - objectiveC' }
SDLOSXPlatform >> sendMessage: sel to: rcv with: aParam [

	^ self ffiCall: #(void* objc_msgSend(void* rcv, void* sel, void* aParam))
]

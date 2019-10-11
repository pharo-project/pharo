"
I execute specific initialization for OSX
"
Class {
	#name : #SDLOSXPlatformInitializer,
	#superclass : #FFILibrary,
	#category : #'OSWindow-SDL2-Bindings'
}

{ #category : #running }
SDLOSXPlatformInitializer >> calloutAPIClass [ 

	^ TFCalloutAPI 
]

{ #category : #running }
SDLOSXPlatformInitializer >> ffiLibraryName [ 

	^ self
]

{ #category : #running }
SDLOSXPlatformInitializer >> lookupClass: aString [

	^ self ffiCall: #(void* objc_lookUpClass(char *aString)) 
]

{ #category : #running }
SDLOSXPlatformInitializer >> lookupSelector: aString [

	^ self ffiCall: #(void* sel_registerName(const char *aString)) 
]

{ #category : #'accessing platform' }
SDLOSXPlatformInitializer >> macModuleName [ 

	^ 'libobjc.dylib'
	
]

{ #category : #running }
SDLOSXPlatformInitializer >> run [ 
	| sel cls |

	sel := self lookupSelector: 'sharedApplication'.
	cls := self lookupClass: 'NSApplication'.
	self sendMessage: sel to: cls
]

{ #category : #running }
SDLOSXPlatformInitializer >> runner [

	^ TFMainThreadRunner uniqueInstance
]

{ #category : #running }
SDLOSXPlatformInitializer >> sendMessage: sel to: cls [

	^ self ffiCall: #(void* objc_msgSend(void* cls, void* sel))
]

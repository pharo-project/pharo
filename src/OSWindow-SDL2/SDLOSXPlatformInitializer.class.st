"
I execute specific initialization for OSX
"
Class {
	#name : #SDLOSXPlatformInitializer,
	#superclass : #Object,
	#category : #'OSWindow-SDL2-Bindings'
}

{ #category : #running }
SDLOSXPlatformInitializer >> lookupClass: aString [

	^ self ffiCall: #(void* objc_lookUpClass(char *aString)) module: 'libobjc.dylib'
]

{ #category : #running }
SDLOSXPlatformInitializer >> lookupSelector: aString [

	^ self ffiCall: #(void* sel_registerName(const char *aString)) module: 'libobjc.dylib'

]

{ #category : #running }
SDLOSXPlatformInitializer >> run [ 
	| sel cls |

	sel := self lookupSelector: 'sharedApplication'.
	cls := self lookupClass: 'NSApplication'.
	self sendMessage: sel to: cls
]

{ #category : #running }
SDLOSXPlatformInitializer >> sendMessage: sel to: cls [

	^ self ffiCall: #(void* objc_msgSend(void* cls, void* sel)) module: 'libobjc.dylib'
]

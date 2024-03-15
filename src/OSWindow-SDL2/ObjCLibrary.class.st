"
I am a little binding for the ObjC bridge library.
I am just used to initialize the minimum the image inside OSX.
"
Class {
	#name : 'ObjCLibrary',
	#superclass : 'FFILibrary',
	#category : 'OSWindow-SDL2-OSX-Specific',
	#package : 'OSWindow-SDL2',
	#tag : 'OSX-Specific'
}

{ #category : 'utilities - objectiveC' }
ObjCLibrary >> class_addMethodClass: cls selector: name implementation: imp signature: types [
	^ self ffiCall: #(int class_addMethod(void* cls, void* name, void* imp, const char *types))
]

{ #category : 'utilities - objectiveC' }
ObjCLibrary >> lookupClass: aString [

	^ self ffiCall: #(void* objc_lookUpClass(char *aString))
]

{ #category : 'utilities - objectiveC' }
ObjCLibrary >> lookupSelector: aString [

	^ self ffiCall: #(void* sel_registerName(const char *aString))
]

{ #category : 'accessing - platform' }
ObjCLibrary >> macLibraryName [

	^ 'libobjc.dylib'
]

{ #category : 'utilities - objectiveC' }
ObjCLibrary >> nsStringOf: aString [

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

{ #category : 'utilities - objectiveC' }
ObjCLibrary >> objc_allocateClassPairSuperclass: superclass name: name extraBytes: extraBytes [
	
	^ self ffiCall: #(void* objc_allocateClassPair(void* superclass, const char *name, size_t extraBytes))
	
]

{ #category : 'utilities - objectiveC' }
ObjCLibrary >> objc_registerClassPair: cls [
	
	self ffiCall: #(void objc_registerClassPair(void* cls))
	
	
]

{ #category : 'utilities - objectiveC' }
ObjCLibrary >> release: aObjCObject [

	| releaseSelector |
	releaseSelector:= self lookupSelector: 'release'.
	self sendMessage: releaseSelector to: aObjCObject
]

{ #category : 'utilities - objectiveC' }
ObjCLibrary >> sendMessage: sel to: cls [

	^ self ffiCall: #(void* objc_msgSend(void* cls, void* sel))
]

{ #category : 'utilities - objectiveC' }
ObjCLibrary >> sendMessage: sel to: rcv with: aParam [

	^ self ffiCall: #(void* objc_msgSend(void* rcv, void* sel, void* aParam))
]

{ #category : 'utilities - objectiveC' }
ObjCLibrary >> sendMessage: sel to: rcv with: aParam1 with:aParam2 with:aParam3 [

	^ self ffiCall: #(void* objc_msgSend(void* rcv, void* sel, void* aParam1, void* aParam2, void* aParam3))
]

{ #category : 'utilities - objectiveC' }
ObjCLibrary >> sendMessage: sel to: rcv withInteger: aParam [

	^ self ffiCall: #(void* objc_msgSend(void* rcv, void* sel, int aParam))
]

{ #category : 'utilities - objectiveC' }
ObjCLibrary >> sendMessage: sel to: rcv withPointer: aParam [

	^ self ffiCall: #(void* objc_msgSend(void* rcv, void* sel, void* aParam))
]

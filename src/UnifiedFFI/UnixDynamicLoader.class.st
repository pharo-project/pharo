"
I'm a unix platform (works in mac too) dynamic library loader.
"
Class {
	#name : #UnixDynamicLoader,
	#superclass : #DynamicLoader,
	#category : #UnifiedFFI-Libraries
}

{ #category : #testing }
UnixDynamicLoader class >> isAvailable [
	^ Smalltalk os isUnix 
		or: [ Smalltalk os isMacOS ]
]

{ #category : #misc }
UnixDynamicLoader >> lastError [
	^ self ffiCall: #(String dlerror(void))
]

{ #category : #misc }
UnixDynamicLoader >> loadLibrary: path [
	^ self loadLibrary: path flag: 0 
]

{ #category : #misc }
UnixDynamicLoader >> loadLibrary: filename flag: flag [
	^ self ffiCall: #(void *dlopen(const char *filename, int flag))
]

{ #category : #misc }
UnixDynamicLoader >> loadSymbolFrom: handle name: symbol [
	^ self ffiCall: #(void *dlsym(void *handle, String symbol))

]

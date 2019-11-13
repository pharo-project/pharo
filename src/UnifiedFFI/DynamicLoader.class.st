"
I'm a class to load libraries and symbols from system.
I provide dlopen in linux and mac and LoadLibrary in windows.
"
Class {
	#name : #DynamicLoader,
	#superclass : #FFILibrary,
	#category : #'UnifiedFFI-Libraries'
}

{ #category : #accessing }
DynamicLoader class >> forCurrentPlatform [
	^ (self allSubclasses 
		detect: [ :each | each isAvailable ])
		uniqueInstance
		
]

{ #category : #testing }
DynamicLoader class >> isAvailable [
	^ self subclassResponsibility
]

{ #category : #misc }
DynamicLoader class >> lastError [
	^ self forCurrentPlatform lastError
]

{ #category : #misc }
DynamicLoader class >> loadLibrary: path [
	^ self forCurrentPlatform loadLibrary: path
]

{ #category : #misc }
DynamicLoader class >> loadLibrary: path flag: flag [
	^ self forCurrentPlatform 
		loadLibrary: path 
		flag: flag
]

{ #category : #misc }
DynamicLoader >> lastError [
	^ self subclassResponsibility
]

{ #category : #misc }
DynamicLoader >> loadLibrary: path [
	^ self subclassResponsibility
]

{ #category : #misc }
DynamicLoader >> loadSymbolFrom: handle name: symbol [
	^ self subclassResponsibility
]

{ #category : #'accessing platform' }
DynamicLoader >> macLibraryName [
	^ 'libc.dylib'
]

{ #category : #'accessing platform' }
DynamicLoader >> unixLibraryName [
	^ 'libdl.so'
]

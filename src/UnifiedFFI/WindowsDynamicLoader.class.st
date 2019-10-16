"
I'm a windows platform dynamic library loader.
"
Class {
	#name : #WindowsDynamicLoader,
	#superclass : #DynamicLoader,
	#category : #'UnifiedFFI-Libraries'
}

{ #category : #testing }
WindowsDynamicLoader class >> isAvailable [
	^ Smalltalk os isWindows
]

{ #category : #misc }
WindowsDynamicLoader >> lastError [
	^ self ffiCall: #(int32 GetLastError(void))
]

{ #category : #misc }
WindowsDynamicLoader >> loadLibrary: lpFileName [
	^ self ffiCall: #(void *LoadLibrary(String lpFileName))
]

{ #category : #misc }
WindowsDynamicLoader >> loadSymbolFrom: hModule name: lpProcName [
	^ self ffiCall: #(void *GetProcAddress(void *hModule, String lpProcName))

]

{ #category : #'accessing platform' }
WindowsDynamicLoader >> win32LibraryName [
	^ 'Kernel32.dll'
]

"
I'm a windows platform dynamic library loader.
"
Class {
	#name : 'WindowsDynamicLoader',
	#superclass : 'DynamicLoader',
	#category : 'UnifiedFFI-Libraries',
	#package : 'UnifiedFFI',
	#tag : 'Libraries'
}

{ #category : 'testing' }
WindowsDynamicLoader class >> isAvailable [
	^ Smalltalk os isWindows
]

{ #category : 'private' }
WindowsDynamicLoader >> basicLoadLibrary: lpFileName [
	
	^ self ffiCall: #(void *LoadLibraryW(Win32WideString lpFileName))
]

{ #category : 'misc' }
WindowsDynamicLoader >> lastError [
	^ self ffiCall: #(int32 GetLastError(void))
]

{ #category : 'misc' }
WindowsDynamicLoader >> loadLibrary: lpFileName [

	^ self basicLoadLibrary: lpFileName asWin32WideString
]

{ #category : 'misc' }
WindowsDynamicLoader >> loadSymbolFrom: hModule name: lpProcName [

	^ self ffiCall: #(void *GetProcAddress(void *hModule, String lpProcName))
]

{ #category : 'accessing - platform' }
WindowsDynamicLoader >> win32LibraryName [
	^ 'Kernel32.dll'
]

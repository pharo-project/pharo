"
I'm used when there is no defined library as a module. 
In case FFI users want to declare modules as simple strings, I'm used to model the access to that library.
I'm never used directly! Instead, users will utilize Strings that will be converted to me thru #asFFILibrary method. 
That way, is completely the same to do a call this way: 

self ffiCall: #( void fn () ) module: 'libc'. 

and 

self ffiCall: #( void fn () ) module: LibC. 

(but of course recomended way is to use a module, in case you need a strategy to find libraries in different platforms. 
"
Class {
	#name : #FFIUnknownLibrary,
	#superclass : #FFILibrary,
	#instVars : [
		'libraryName'
	],
	#category : #'UnifiedFFI-Libraries'
}

{ #category : #'instance creation' }
FFIUnknownLibrary class >> name: aString [ 
	^ self basicNew
		initializeName: aString;
		yourself
]

{ #category : #'instance creation' }
FFIUnknownLibrary class >> new [ 
	self error: 'Use #name:'
]

{ #category : #initialization }
FFIUnknownLibrary >> initializeName: aString [ 
	libraryName := aString.
	self initialize
]

{ #category : #accessing }
FFIUnknownLibrary >> libraryName [
	^ libraryName
]

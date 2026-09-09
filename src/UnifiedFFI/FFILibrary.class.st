"
I'm an util to answer module libraries that can have different names in different platforms. 
I just implement a double dispatch with platform.
"
Class {
	#name : 'FFILibrary',
	#superclass : 'Object',
	#classInstVars : [
		'uniqueInstance'
	],
	#category : 'UnifiedFFI-Libraries',
	#package : 'UnifiedFFI',
	#tag : 'Libraries'
}

{ #category : 'converting' }
FFILibrary class >> asFFILibrary [
	^ self uniqueInstance
]

{ #category : 'converting' }
FFILibrary class >> calloutAPIClass [

	^ self uniqueInstance calloutAPIClass
]

{ #category : 'instance creation' }
FFILibrary class >> new [
	self error: 'Use #uniqueInstance'
]

{ #category : 'options' }
FFILibrary class >> options [

	^ #( #+ optStrict )
]

{ #category : 'instance creation' }
FFILibrary class >> uniqueInstance [
	self = FFILibrary
		ifTrue: [ self error: 'I''m an abstract class, use one of my children.' ].
	^ uniqueInstance ifNil: [ uniqueInstance := super new ]
]

{ #category : 'converting' }
FFILibrary >> asFFILibrary [
	^ self
]

{ #category : 'converting' }
FFILibrary >> calloutAPIClass [

	^ FFICalloutAPI calloutAPIClass
]

{ #category : 'calling' }
FFILibrary >> ffiCall: fnSpec [
	<ffiCalloutTranslator>

	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		options: self ffiLibrary options;
		function: fnSpec module: self
]

{ #category : 'calling' }
FFILibrary >> ffiCall: fnSpec options: callOptions [
	<ffiCalloutTranslator>

	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		options: (self ffiLibrary options), callOptions;
		function: fnSpec module: self
]

{ #category : 'accessing' }
FFILibrary >> ffiLibrary [

	"This method returns the FFILibrary to use when executing a FFI call.
	As we are already in a FFILibrary, the ffi call can use myself!"

	^ self
]

{ #category : 'library path' }
FFILibrary >> ffiLibraryName [
	^ self libraryName
]

{ #category : 'accessing' }
FFILibrary >> libraryName [
	^ OSPlatform current ffiLibraryName: self
]

{ #category : 'accessing - platform' }
FFILibrary >> macLibraryName [

	^ self subclassResponsibility
]

{ #category : 'accessing - platform' }
FFILibrary >> options [

	^ self class options
]

{ #category : 'extension - hooks' }
FFILibrary >> postMethodBuildContext: sender builder: builder spec: functionSpec [
]

{ #category : 'extension - hooks' }
FFILibrary >> preMethodBuildContext: sender builder: builder spec: functionSpec [
]

{ #category : 'accessing' }
FFILibrary >> uniqueInstance [

	^ self
]

{ #category : 'accessing - platform' }
FFILibrary >> unix32LibraryName [

	^ self unixLibraryName
]

{ #category : 'accessing - platform' }
FFILibrary >> unix64LibraryName [

	^ self unixLibraryName
]

{ #category : 'accessing - platform' }
FFILibrary >> unixLibraryName [
	"Users should use unix32* or unix64*"

	^ self subclassResponsibility
]

{ #category : 'accessing - platform' }
FFILibrary >> win32LibraryName [

	^ self subclassResponsibility
]

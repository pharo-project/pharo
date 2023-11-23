"
A test object used in UFFI functional tests
"
Class {
	#name : 'FFITestObject',
	#superclass : 'FFIExternalObject',
	#instVars : [
		'a',
		'c'
	],
	#category : 'UnifiedFFI-Tests-Tests',
	#package : 'UnifiedFFI-Tests',
	#tag : 'Tests'
}

{ #category : 'accessing' }
FFITestObject >> a [

	 ^a
]

{ #category : 'accessing' }
FFITestObject >> b: _b d: d_ [

	"some method to bind an ffi callout"
]

{ #category : 'accessing' }
FFITestObject >> c [

	 ^c
]

{ #category : 'library path' }
FFITestObject >> ffiLibraryName [

	^ 'testLibrary'
]

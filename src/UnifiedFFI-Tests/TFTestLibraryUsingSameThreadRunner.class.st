"
I am a test library using the SameThread runner
"
Class {
	#name : 'TFTestLibraryUsingSameThreadRunner',
	#superclass : 'FFILibrary',
	#category : 'UnifiedFFI-Tests-Tests',
	#package : 'UnifiedFFI-Tests',
	#tag : 'Tests'
}

{ #category : 'accessing - platform' }
TFTestLibraryUsingSameThreadRunner >> calloutAPIClass [

	^ TFCalloutAPI
]

{ #category : 'accessing - platform' }
TFTestLibraryUsingSameThreadRunner >> macLibraryName [

	^ 'libTestLibrary.dylib'
]

{ #category : 'accessing - platform' }
TFTestLibraryUsingSameThreadRunner >> unix64LibraryName [

	^ 'libTestLibrary.so'
]

{ #category : 'accessing - platform' }
TFTestLibraryUsingSameThreadRunner >> win32LibraryName [

	^ 'TestLibrary.dll'
]

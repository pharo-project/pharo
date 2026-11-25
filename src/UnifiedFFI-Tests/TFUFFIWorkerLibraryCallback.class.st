"
I am a callback that declares its signature and library, to test FFICallback class>>#on:.
"
Class {
	#name : 'TFUFFIWorkerLibraryCallback',
	#superclass : 'FFICallback',
	#category : 'UnifiedFFI-Tests-Tests',
	#package : 'UnifiedFFI-Tests',
	#tag : 'Tests'
}

{ #category : 'accessing' }
TFUFFIWorkerLibraryCallback class >> ffiLibraryName [

	^ TFTestLibraryUsingWorker
]

{ #category : 'accessing' }
TFUFFIWorkerLibraryCallback class >> fnSpec [

	^ #( int #( int a ) )
]

"
I am a callback that declares its signature and library, to test FFICallback class>>#on:.
"
Class {
	#name : 'TFUFFIWorkerLibraryCallback',
	#superclass : 'FFICallback',
	#category : 'ThreadedFFI-UFFI-Tests',
	#package : 'ThreadedFFI-UFFI-Tests'
}

{ #category : 'accessing' }
TFUFFIWorkerLibraryCallback class >> ffiLibraryName [

	^ TFTestLibraryUsingWorker
]

{ #category : 'accessing' }
TFUFFIWorkerLibraryCallback class >> fnSpec [

	^ #( int #( int a ) )
]

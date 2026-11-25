"
I am a test library using a worker runner
"
Class {
	#name : 'TFTestLibraryUsingWorker',
	#superclass : 'TFTestLibraryUsingSameThreadRunner',
	#category : 'UnifiedFFI-Tests-Tests',
	#package : 'UnifiedFFI-Tests',
	#tag : 'Tests'
}

{ #category : 'accessing' }
TFTestLibraryUsingWorker >> runner [

	^ TFWorker default
]

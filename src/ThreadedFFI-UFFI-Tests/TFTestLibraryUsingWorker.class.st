"
I am a test library using a worker runner
"
Class {
	#name : #TFTestLibraryUsingWorker,
	#superclass : #TFTestLibraryUsingSameThreadRunner,
	#category : #'ThreadedFFI-UFFI-Tests'
}

{ #category : #accessing }
TFTestLibraryUsingWorker >> runner [ 
	
	^ TFWorker default
]

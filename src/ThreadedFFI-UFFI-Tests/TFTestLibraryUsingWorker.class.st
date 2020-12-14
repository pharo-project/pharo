Class {
	#name : #TFTestLibraryUsingWorker,
	#superclass : #TFTestLibraryUsingSameThreadRunner,
	#category : #'ThreadedFFI-UFFI-Tests'
}

{ #category : #accessing }
TFTestLibraryUsingWorker >> runner [ 
	
	^ TFWorker default
]

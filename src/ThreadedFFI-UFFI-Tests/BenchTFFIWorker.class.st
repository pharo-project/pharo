"
I am a simple benchmark class to test the TFFIWorker scheme
"
Class {
	#name : #BenchTFFIWorker,
	#superclass : #BenchTFFISameThread,
	#category : #'ThreadedFFI-UFFI-Tests-Benchs'
}

{ #category : #accessing }
BenchTFFIWorker >> ffiLibrary [

	^ TFTestLibraryUsingWorker uniqueInstance
]

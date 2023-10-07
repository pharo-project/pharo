"
I am a simple benchmark class to test the TFFIWorker scheme
"
Class {
	#name : 'BenchTFFIWorker',
	#superclass : 'BenchTFFISameThread',
	#category : 'ThreadedFFI-UFFI-Tests-Benchs',
	#package : 'ThreadedFFI-UFFI-Tests',
	#tag : 'Benchs'
}

{ #category : 'accessing' }
BenchTFFIWorker >> ffiLibrary [

	^ TFTestLibraryUsingWorker uniqueInstance
]

Class {
	#name : #BenchTFFIWorker,
	#superclass : #BenchTFFISameThread,
	#category : #'ThreadedFFI-UFFI-Tests-Benchs'
}

{ #category : #accessing }
BenchTFFIWorker >> doCallWith: aFloat another: aDouble [

	^ self ffiCall: #(float sumAFloatAndADouble(float aFloat, double aDouble))
]

{ #category : #accessing }
BenchTFFIWorker >> ffiLibrary [

	^ TFTestLibraryUsingWorker uniqueInstance
]

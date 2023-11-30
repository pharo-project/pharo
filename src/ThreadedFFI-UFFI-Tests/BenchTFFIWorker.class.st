"
I am a simple benchmark class to test the TFFIWorker scheme
"
Class {
	#name : 'BenchTFFIWorker',
	#superclass : 'BenchTFFI',
	#category : 'ThreadedFFI-UFFI-Tests-Benchs',
	#package : 'ThreadedFFI-UFFI-Tests',
	#tag : 'Benchs'
}

{ #category : 'private' }
BenchTFFIWorker >> doCallWith: aFloat another: aDouble [

	^ self ffiCall: #(float sumAFloatAndADouble(float aFloat, double aDouble))
]

{ #category : 'accessing' }
BenchTFFIWorker >> ffiLibrary [

	^ TFTestLibraryUsingWorker uniqueInstance
]

{ #category : 'running' }
BenchTFFIWorker >> runCall [
	<script: 'self new runCall traceCr'>

	^ super runCall
]

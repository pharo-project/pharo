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

{ #category : 'private' }
BenchTFFIWorker >> doSumWithPointers [

	^ self ffiCall: #(int64 sum_int64_t(void* 1, void* 0))
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

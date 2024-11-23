"
I am an abstract base for concrete TFFI benchmarks, expressed on my subclasses.
"
Class {
	#name : 'BenchTFFI',
	#superclass : 'Object',
	#category : 'ThreadedFFI-UFFI-Tests-Benchs',
	#package : 'ThreadedFFI-UFFI-Tests',
	#tag : 'Benchs'
}

{ #category : 'accessing' }
BenchTFFI >> calloutAPIClass [

	^ TFCalloutAPI
]

{ #category : 'private' }
BenchTFFI >> doCallWith: aFloat another: aDouble [

	^ self ffiCall: #(float sumAFloatAndADouble(float aFloat, double aDouble))
]

{ #category : 'private' }
BenchTFFI >> doSumWithPointers [

	^ self ffiCall: #(int64 sum_int64_t(void* 1, void* 0))

	
]

{ #category : 'accessing' }
BenchTFFI >> ffiLibrary [

	^ self subclassResponsibility
]

{ #category : 'running' }
BenchTFFI >> runCall [

	| return |
	^ [ return := self doCallWith: 1.0 another: 2.0.
		 self assert: return = 3.0 ] bench
]

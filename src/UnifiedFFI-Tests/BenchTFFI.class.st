"
I am an abstract base for concrete TFFI benchmarks, expressed on my subclasses.
"
Class {
	#name : 'BenchTFFI',
	#superclass : 'Object',
	#category : 'UnifiedFFI-Tests-Benchs',
	#package : 'UnifiedFFI-Tests',
	#tag : 'Benchs'
}

{ #category : 'accessing' }
BenchTFFI >> calloutAPIClass [

	^ TFCalloutAPI
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

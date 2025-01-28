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

{ #category : 'running' }
BenchTFFI >> runCallWithByteArray [ 

	^ [ self doCallWithByteArray: (ByteArray new: 17)  ] bench
]

{ #category : 'running' }
BenchTFFI >> runCallWithExternalAddress [ 

	^ [ self doCallWithExternalAddress: ExternalAddress null  ] bench
]

{ #category : 'running' }
BenchTFFI >> runCallWithPointerVoid [

	^ [ self doPointerVoid: (ExternalAddress null)  ] bench
]

{ #category : 'running' }
BenchTFFI >> runCallWithPointerVoidWithByteArray [

	^ [ self doPointerVoid: (ByteArray new: 17)  ] bench
]

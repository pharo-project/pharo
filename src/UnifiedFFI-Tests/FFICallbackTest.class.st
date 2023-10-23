"
Tests for callbacks
"
Class {
	#name : 'FFICallbackTest',
	#superclass : 'TestCase',
	#pools : [
		'FFITestEnumeration'
	],
	#category : 'UnifiedFFI-Tests-Tests',
	#package : 'UnifiedFFI-Tests',
	#tag : 'Tests'
}

{ #category : 'accessing' }
FFICallbackTest >> floatregargsp [
	^ nil
]

{ #category : 'accessing' }
FFICallbackTest >> intregargsp [
	^ nil
]

{ #category : 'running' }
FFICallbackTest >> runCaseManaged [
	^ self runCase
]

{ #category : 'tests' }
FFICallbackTest >> testCqsort [
	| unsorted expected values callback results |

	unsorted := (10.0 to: 1.0 by: -0.5) asArray.
	expected := (1.0 to: 10.0 by: 0.5) asArray.

	values := FFIExternalArray externalNewType: 'double' size: unsorted size.
	unsorted withIndexDo: [ :each :index | values at: index put: each ].
	callback := FFICallback
		signature:  #(int (const void *arg1, const void *arg2))
		block: [ :arg1 :arg2 | ((arg1 doubleAt: 1) - (arg2 doubleAt: 1)) sign ].
	[
		FFICallback
			primQsort: values
			with: unsorted size
			with: values type typeSize
			with: callback.
		results := values asArray ]
	ensure: [ values free ].

	self assert: results equals: expected
]

{ #category : 'tests' }
FFICallbackTest >> testCqsortWithByteArray [
	| unsorted expected values callback results |

	unsorted := (10.0 to: 1.0 by: -0.5) asArray.
	expected := (1.0 to: 10.0 by: 0.5) asArray.

	values := FFIExternalArray newType: 'double' size: unsorted size.
	values getHandle pinInMemory.
	unsorted withIndexDo: [ :each :index | values at: index put: each ].
	callback := FFICallback
		signature:  #(int (const void *arg1, const void *arg2))
		block: [ :arg1 :arg2 | ((arg1 doubleAt: 1) - (arg2 doubleAt: 1)) sign ].
	[
		FFICallback
			primQsort: values
			with: unsorted size
			with: values type typeSize
			with: callback.
		results := values asArray ]
	ensure: [ values free ].

	self assert: results equals: expected
]

{ #category : 'support' }
FFICallbackTest >> wordResult: anInteger [
	"For testEnumerationReturnValue, simulate a method invoked
	from FFICallback>>valueInContext, so that I can be passed as a fake context."
	^anInteger
]

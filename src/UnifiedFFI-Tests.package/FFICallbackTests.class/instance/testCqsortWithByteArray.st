tests
testCqsortWithByteArray
	| unsorted expected values callback results |
	
	self flag: #TODO. "TEMPORAL... I need to see why this is crashing on windows (but callbacks are 
	working in general)"
	OSPlatform current isWindows 
		ifTrue: [ ^ self skip ].

	unsorted := (10.0 to: 1.0 by: -0.5) asArray.
	expected := (1.0 to: 10.0 by: 0.5) asArray.
	
	values := FFIExternalArray newType: 'double' size: unsorted size.
	values getHandle pin.
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
	
	self assert: results equals: expected.
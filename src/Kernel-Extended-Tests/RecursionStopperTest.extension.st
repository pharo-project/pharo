Extension { #name : 'RecursionStopperTest' }

{ #category : '*Kernel-Extended-Tests' }
RecursionStopperTest >> testThreadSafe [

	self threadSafe.

	self assert: self fork isNil
]

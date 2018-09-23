Extension { #name : #RecursionStopperTest }

{ #category : #'*Kernel-Tests-Extended' }
RecursionStopperTest >> testThreadSafe [

	self should: [ self threadSafe ] notTakeMoreThanMilliseconds: 10.
	
	self assert: fork isNil.
]

Extension { #name : #FloatTest }

{ #category : #'*Kernel-Tests-Extended' }
FloatTest >> testRounding [

	self assert: (10.1234 round: 2) equals: 10.12.
 	self assert: (10.1234 round: 0) equals: 10
]

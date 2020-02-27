Extension { #name : #NumberTest }

{ #category : #'*Kernel-Tests-Extended' }
NumberTest >> testPercent [
	self assert: 20 / 40 equals: 50 percent
]

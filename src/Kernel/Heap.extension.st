Extension { #name : #Heap }

{ #category : #'*Kernel-Processes' }
Heap >> removeFirstOrNil [
	"Remove the first element from the receiver"
	self isEmpty ifTrue:  [ ^nil ].
	^self privateRemoveAt: 1
]

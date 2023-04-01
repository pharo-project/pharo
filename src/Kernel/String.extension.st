Extension { #name : #String }

{ #category : #'*Kernel' }
String >> isUnclassified [
	"Return true if I am the name of unclassified protocol"

	^ self = Protocol unclassified
]

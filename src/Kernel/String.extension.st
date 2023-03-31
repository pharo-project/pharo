Extension { #name : #String }

{ #category : #'*Kernel' }
String >> isUnclassified [
	"Return true if I am the name of Uncliassified protocol"

	^ self = Protocol unclassified
]

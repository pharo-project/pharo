Extension { #name : 'DoubleLinkedList' }

{ #category : '*System-Finalization' }
DoubleLinkedList >> checkIntegrity [
	"Validate that the receiver looks OK.
	Assumes that the receiver is not modified while running the check."
	| currentLink previousLink |

	self isEmpty ifTrue: [ ^ self ].

	"If the receiver is not empty, neither head nor tail should be nil"
	self assert: head isNotNil
		description: 'Not empty, but head is nil'.
	self assert: tail isNotNil
		description: 'Not empty, but tail is nil'.

	self assert: head hasPredecessor not.
	self assert: tail hasSuccessor not.

	currentLink := head.
	previousLink := nil.
	[ currentLink nextLink isNotNil ] whileTrue:
		[ self assert: currentLink previousLink == previousLink
			description: 'previousLink broken'.
		previousLink := currentLink.
		currentLink := currentLink nextLink. ].
	self assert: currentLink == tail
		description: 'last link is not tail'.
]

{ #category : '*System-Finalization' }
DoubleLinkedList >> hasIntegrity [
	"Answer a boolean indicating the receiver looks OK.
	Assumes that the receiver is not modified while running the check."

	^ [ self checkIntegrity.
		true ]
			on: AssertionFailure
			do: [ :ex | false ]
]

{ #category : '*System-Finalization' }
DoubleLinkedList >> removeMaybeNotExistingLink: link [
	"Remove the specified link.
	Checks if the link is already removed.
	This is safe to be used when removing the link from outside the collection.
	The collection should use #removeLink: as it will be faster.
	Returns true if it was removed."

	| predecessor successor |
	predecessor := link previousLink.
	successor := link nextLink.
	predecessor
		ifNil: [
				self head = link ifFalse: [
						link clearLinks.
						^ false ].
				self head: successor ]
		ifNotNil: [
				predecessor nextLink = link ifFalse: [
						link clearLinks.
						^ false ].
				predecessor nextLink: successor ].
	successor ifNil: [ self tail: predecessor ] ifNotNil: [ successor previousLink: predecessor ].

	link clearLinks.

	^ true
]

as yet unclassified
instSpecOfClass: aRFiClass

	"NEW:
	 0 = 0 sized objects (UndefinedObject True False et al)
	 1 = non-indexable objects with inst vars (Point et al)
	 2 = indexable objects with no inst vars (Array et al)
	 3 = indexable objects with inst vars (MethodContext AdditionalMethodState et al)
	 4 = weak indexable objects with inst vars (WeakArray et al)
	 5 = weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
	 6 = reserved
	 7 = forwarder format (also immediate class format)
	 9          64-bit indexable
	 10 - 11 32-bit indexable
	 12 - 15 16-bit indexable
	 16 - 23 byte indexable
	 24 - 31 compiled method"
	(aRFiClass isBytes) ifTrue: [ ^ 16 ].
	(aRFiClass isWords) ifTrue: [ ^ 10 ].
	(aRFiClass isCompiledMethod) ifTrue: [ ^ 24 ].
	(aRFiClass isEphemeron) ifTrue: [ ^ 5 ].
	(aRFiClass instSize = 0 and: [ aRFiClass isVariable not ]) ifTrue: [ ^ 0 ].
	(aRFiClass instSize > 0 and: [ aRFiClass isVariable not ]) ifTrue: [ ^ 1 ].
	(aRFiClass instSize = 0 and: [ aRFiClass isVariable     ]) ifTrue: [ ^ 2 ].
	(aRFiClass instSize > 0 and: [ aRFiClass isVariable     ]) ifTrue: [ ^ 3 ].
	(aRFiClass instSize > 0 and: [ aRFiClass isVariable and: [ aRFiClass isWeak ] ]) ifTrue: [ ^ 4 ].
	(aRFiClass instSize > 0 and: [ aRFiClass isVariable not and: [ aRFiClass isWeak ] ]) ifTrue: [ ^ 5 ].
	self error: 'illegal old format'
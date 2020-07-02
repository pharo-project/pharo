Extension { #name : #SharedQueue }

{ #category : #'*TaskIt' }
SharedQueue >> fixedSize: aSize nextPut: anObject [
	monitor
		critical:
			[ [ aSize - items size < 1 ] whileTrue: [ items removeFirst ].
			items addLast: anObject.
			monitor signal ].
	^ anObject
]

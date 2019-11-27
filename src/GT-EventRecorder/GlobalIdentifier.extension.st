Extension { #name : #GlobalIdentifier }

{ #category : #'*GT-EventRecorder' }
GlobalIdentifier >> obfuscateIfNecessary: anObject [
	^ Privacy sendEntityNames
			ifTrue: [ anObject ]
			ifFalse: [ self obfuscate: anObject ]
]

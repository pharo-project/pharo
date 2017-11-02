Extension { #name : #Symbol }

{ #category : #'*System-Support' }
Symbol >> implementors [
	^SystemNavigation new allImplementorsOf: self
]

{ #category : #'*System-Support' }
Symbol >> senders [
	^SystemNavigation new allSendersOf: self
]

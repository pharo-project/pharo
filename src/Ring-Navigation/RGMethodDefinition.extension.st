Extension { #name : #RGMethodDefinition }

{ #category : #'*Ring-Navigation' }
RGMethodDefinition >> implementors [
	
	^ SystemNavigation default allImplementorsOf: self selector
]

{ #category : #'*Ring-Navigation' }
RGMethodDefinition >> senders [
	
	^ SystemNavigation default allSendersOf: self selector
]

Extension { #name : #Symbol }

{ #category : #'*Clap-Core' }
Symbol >> asClapIdentifier [
	^ self
]

{ #category : #'*Clap-Core' }
Symbol >> identifiesClapParameter: aClapParameter [
	^ self = aClapParameter identifier
]

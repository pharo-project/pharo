Extension { #name : #LiteralVariable }

{ #category : #'*Reflectivity' }
LiteralVariable >> isCascade [
	self flag: #hack.
	^false
]

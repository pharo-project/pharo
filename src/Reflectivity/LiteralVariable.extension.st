Extension { #name : #LiteralVariable }

{ #category : #'*Reflectivity' }
LiteralVariable >> isCascade [
	self flag: #hack.
	^false
]

{ #category : #'*Reflectivity' }
LiteralVariable >> methodNode [
	self flag: #hack.
	^nil
]

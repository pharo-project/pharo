Extension { #name : #RBMessageNode }

{ #category : #'*renraku' }
RBMessageNode >> leftmostChainReceiver [

	^ self receiver isMessage
		ifTrue: [ self receiver receiver ]
		ifFalse: [ self receiver ]
]

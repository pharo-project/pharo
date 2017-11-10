Extension { #name : #RBCascadeNode }

{ #category : #'*renraku' }
RBCascadeNode >> leftmostChainReceiver [

	^ self receiver isMessage
		ifTrue: [ self receiver receiver ]
		ifFalse: [ self receiver ]
]

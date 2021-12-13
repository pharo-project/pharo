Extension { #name : #RBAnonymousVisitor }

{ #category : #'*ClassParser' }
RBAnonymousVisitor >> visitSlotInitializationNode: slotInitializationNode [

	visits
		at: #slotInitializationNode
		ifPresent: [ :aBlock | aBlock cull: slotInitializationNode ].
	^ super visitSlotInitializationNode: slotInitializationNode
]

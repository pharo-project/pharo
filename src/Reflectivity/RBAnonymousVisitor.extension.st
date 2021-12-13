Extension { #name : #RBAnonymousVisitor }

{ #category : #'*Reflectivity' }
RBAnonymousVisitor >> visitStoreIntoTempNode: storeIntoTempNode [

	visits
		at: #storeIntoTempNode
		ifPresent: [ :aBlock | aBlock cull: storeIntoTempNode ].
	^ super visitStoreIntoTempNode: storeIntoTempNode
]

{ #category : #'*Reflectivity' }
RBAnonymousVisitor >> visitStorePopIntoTempNode: storePopIntoTempNode [

	visits
		at: #storePopIntoTempNode
		ifPresent: [ :aBlock | aBlock cull: storePopIntoTempNode ].
	^ super visitStorePopIntoTempNode: storePopIntoTempNode
]

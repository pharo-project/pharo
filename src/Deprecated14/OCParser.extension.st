Extension { #name : 'OCParser' }

{ #category : '*Deprecated14' }
OCParser >> errorBlock [

	self deprecated: 'see errorBlock:'
]

{ #category : '*Deprecated14' }
OCParser >> errorBlock: aBlock [

	self deprecated: 'RBParser instances offer no more errorBlock facility. Use helpers methods on the class side, or `checkFaulty: aBlock` on a parsed AST'
]

Extension { #name : 'OCParser' }

{ #category : '*Deprecated14' }
OCParser >> errorBlock [

	self
		deprecated: 'see errorBlock:'
		in: #Pharo14
]

{ #category : '*Deprecated14' }
OCParser >> errorBlock: aBlock [

	self
		deprecated: 'RBParser instances offer no more errorBlock facility. Use helpers methods on the class side, or `checkFaulty: aBlock` on a parsed AST'
		in: #Pharo14
]

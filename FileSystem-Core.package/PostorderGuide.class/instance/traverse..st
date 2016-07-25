showing
traverse: anEntry 
		
	self pushVisit: anEntry.
	
	anEntry isDirectory ifTrue: [ 
		(self shouldVisitChildrenOf: anEntry)
			ifFalse: [ ^ self ].
		anEntry reference entries reverseDo: [ :each | 
			self pushTraverse: each ]]
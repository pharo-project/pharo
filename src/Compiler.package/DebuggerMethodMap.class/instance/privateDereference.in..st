private
privateDereference: tempReference in: aContext
	"Fetch the temporary with reference tempReference in aContext.
	 tempReference can be
		integer - direct temp reference
		#( indirectionVectorIndex tempIndex ) - remote temp in indirectionVector at index
		#( outer. temp reference ) - a temp reference in an outer context."
	^tempReference isInteger
		ifTrue: [aContext tempAt: tempReference]
		ifFalse:
			[tempReference first == #outer
				ifTrue: [self privateDereference: tempReference last
							in: aContext outerContext]
				ifFalse: [(aContext tempAt: tempReference first)
							at: tempReference second]]
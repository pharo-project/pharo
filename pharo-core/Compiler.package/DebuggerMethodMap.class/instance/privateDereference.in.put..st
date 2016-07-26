private
privateDereference: tempReference in: aContext put: aValue
	"Assign the temporary with reference tempReference in aContext.
	 tempReference can be
		integer - direct temp reference
		#( indirectionVectorIndex tempIndex ) - remote temp in indirectionVector at index
		#( outer. temp reference ) - a temp reference in an outer context."
	^tempReference isInteger
		ifTrue: [aContext tempAt: tempReference put: aValue]
		ifFalse:
			[tempReference first == #outer
				ifTrue: [self privateDereference: tempReference last
							in: aContext outerContext
							put: aValue]
				ifFalse: [(aContext tempAt: tempReference first)
							at: tempReference second
							put: aValue]]
paragraph feedbacks
paragraphWasComposedFrom: startIndex to: stopIndex
	self
		handleBoundsChange: [ 
			self wrapped
				ifTrue: [ self updateBottomFromParagraph ]
				ifFalse: [ self updateExtentFromParagraph ] ]
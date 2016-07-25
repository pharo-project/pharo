geometry
extent: aPoint
	self
		handleBoundsChange: [ 
			self wrapped
				ifTrue: [ 
					(bounds notNil and: [ bounds width = aPoint x ])
						ifTrue: [ ^ self ].
					super extent: ((self paragraph extentFromClientBottomRight: aPoint) max: self minimumExtent).
					self recomputeSelection ] ]
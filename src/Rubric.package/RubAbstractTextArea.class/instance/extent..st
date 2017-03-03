geometry
extent: aPoint
	self
		handleBoundsChange: [ 
			self wrapped
				ifTrue: [ 
					(bounds notNil and: [ bounds width = aPoint x ])
						ifTrue: [ ^ self ].
					super extent: ((self paragraph withoutDecorator extentFromClientBottomRight: aPoint) max: self minimumExtent).
					self recomputeSelection ] ]
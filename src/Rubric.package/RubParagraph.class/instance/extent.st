geometry
extent
	^ self actualWidth
		ifNil: [ self minimumExtent ]
		ifNotNil: [ 
			(self actualWidth + self margins left + self margins right)
				@ (self lines last bottom - self lines first top + self margins top + self margins bottom) ]
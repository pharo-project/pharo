private
intersectSmallWithLarge: aLargerSet
	^ self select: [ :each | (aLargerSet includes: each) ]
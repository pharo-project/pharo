testing
isZero

	^ (0 = top) 
		and: [ (0 = left) 
				and: [ (0 = right) 
					and: [ 0 = bottom]]]
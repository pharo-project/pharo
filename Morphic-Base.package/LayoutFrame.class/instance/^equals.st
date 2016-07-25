comparing
= aFrame
	^self species = aFrame species
		and:[ self leftFraction == aFrame leftFraction
		and:[ self leftOffset == aFrame leftOffset
		and:[ self topFraction == aFrame topFraction
		and:[ self topOffset == aFrame topOffset
		and:[ self rightFraction == aFrame rightFraction
		and:[ self rightOffset == aFrame rightOffset
		and:[ self bottomFraction == aFrame bottomFraction
		and:[ self bottomOffset == aFrame bottomOffset]]]]]]]]
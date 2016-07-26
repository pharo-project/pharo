scroll cache
deriveHScrollRange

	|  unadjustedRange totalRange |
	(list isNil or: [list isEmpty]) 
		ifTrue:[hScrollRangeCache := Array with: 0 with: 0 with: 0 with: 0 with: 0 ]
		ifFalse:[ 
			unadjustedRange := self listMorph hUnadjustedScrollRange.
			totalRange := unadjustedRange + self hExtraScrollRange + self hMargin.
			hScrollRangeCache := Array 
										with: totalRange 
										with: unadjustedRange 
										with: list size 
										with: list first 
										with: list last .
		].

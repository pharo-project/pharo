comparing
= aNautilus

	self class = aNautilus class ifFalse: [ ^ false ].
	1 to: self class instVarNames size do: [:index |
		(self instVarAt: index) = (aNautilus instVarAt: index)
			ifFalse: [ ^ false ]].
	^ true
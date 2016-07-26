comparing
= aMatrix
	^ aMatrix class == self class
		and: [ aMatrix numberOfRows = numberOfRows
				and: [ aMatrix numberOfColumns = numberOfColumns 
					and: [ aMatrix privateContents = contents ] ] ]
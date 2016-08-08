comparing
= aGrid
	^ aGrid class == self class
		and: [ aGrid numberOfRows = numberOfRows
				and: [ aGrid numberOfColumns = numberOfColumns 
					and: [ aGrid privateContents = contents ] ] ]
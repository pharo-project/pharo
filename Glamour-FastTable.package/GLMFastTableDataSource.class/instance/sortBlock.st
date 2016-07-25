private
sortBlock
	^ columnToSortBy sortBlock ifNil: [ 
		columnToSortBy defaultSortBlockForPresentation: self glamourPresentation ]
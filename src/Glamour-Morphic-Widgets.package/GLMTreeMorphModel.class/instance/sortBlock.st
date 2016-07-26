callbacks
sortBlock

	^ columnToSortBy sortBlock 
		ifNotNil: [ :aBlock | aBlock ]
		ifNil: [ 
			columnToSortBy defaultSortBlockForPresentation: self glamourPresentation ]
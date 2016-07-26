searching
listForSearching

	^ getListSelector
		ifNotNil: [ self getList ]
		ifNil: [ getListElementSelector
				ifNil: [ #() ]
				ifNotNil: [ 
					(1 to: self getListSize) collect: [:index | self getListItem: index ]]]
			
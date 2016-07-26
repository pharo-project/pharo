searching
selectSearchedElement

	self searchedElement ifNotNil: [: index |	
			 ActiveEvent commandKeyPressed	ifFalse: [ self resetListSelectionSilently ].
			self changeModelSelection: index.
			self isMultipleSelection
				ifTrue: [ self listSelectionAt: index put: true ].
			self vScrollValue: ((index-1)/self getListSize) ]
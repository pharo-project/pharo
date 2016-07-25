menus
classListMenu: aMenu 

	^aMenu addList: #(
		-
		('Browse full (b)'			browseMethodFull)
		('Browse hierarchy (h)'		classHierarchy)
		-
		('Show hierarchy'			methodHierarchy))
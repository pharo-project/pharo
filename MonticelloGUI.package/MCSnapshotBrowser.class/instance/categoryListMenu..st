menus
categoryListMenu: aMenu 
	categorySelection
		ifNotNil: [aMenu
				add: (categorySelection = '*Extensions'
						ifTrue: ['Load all extension methods']
						ifFalse: ['Load class category {1}' format: {categorySelection}])
				selector: #loadCategorySelection].
	^ aMenu
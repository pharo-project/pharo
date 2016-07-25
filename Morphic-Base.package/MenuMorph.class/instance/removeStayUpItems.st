menu
removeStayUpItems

	self menuItems
		select: [ :menuItem | menuItem isStayUpItem ]
		thenDo: [ :each | each delete ].

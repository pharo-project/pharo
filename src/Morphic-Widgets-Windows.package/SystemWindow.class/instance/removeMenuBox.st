geometry
removeMenuBox
	menuBox ifNotNil:
		[menuBox delete.
		menuBox := nil].

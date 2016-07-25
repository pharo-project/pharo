ui-building
setColor: aColor
	self color: aColor.
	self choiceMenus 
		ifNotNil: [:menus | menus 
			do: [:cm | cm color: aColor]]
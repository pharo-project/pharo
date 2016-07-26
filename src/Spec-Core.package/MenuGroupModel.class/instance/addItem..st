api
addItem: aBlock
	| item |
	
	item := MenuItemModel new.
	aBlock value: item.
	self addMenuItem: item
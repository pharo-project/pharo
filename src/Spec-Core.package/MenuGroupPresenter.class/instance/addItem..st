api
addItem: aBlock
	| item |
	
	item := MenuItemPresenter new.
	aBlock value: item.
	self addMenuItem: item
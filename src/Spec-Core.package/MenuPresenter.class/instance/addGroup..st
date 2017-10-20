api
addGroup: aBlock
	| group |
	
	group := MenuGroupPresenter new.
	aBlock value: group.
	self addMenuGroup: group
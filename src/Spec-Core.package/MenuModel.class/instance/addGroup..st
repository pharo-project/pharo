api
addGroup: aBlock
	| group |
	
	group := MenuGroupModel new.
	aBlock value: group.
	self addMenuGroup: group
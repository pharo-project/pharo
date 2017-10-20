private
addMenuGroup: aMenuGroup

	groups add: aMenuGroup.
	self autoRefresh 
		ifNotNil: [ aMenuGroup autoRefresh: self autoRefresh ]
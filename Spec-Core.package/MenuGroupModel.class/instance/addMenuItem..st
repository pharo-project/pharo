private
addMenuItem: aMenuItem

	items add: aMenuItem.
	self autoRefresh 
		ifNotNil: [ aMenuItem autoRefresh: self autoRefresh ]
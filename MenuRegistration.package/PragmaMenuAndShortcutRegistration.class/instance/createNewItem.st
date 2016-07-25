menu protocol
createNewItem
	
	items add: self itemClass new.
	
	self currentItem
		platform: platform;
		parent: currentRoot
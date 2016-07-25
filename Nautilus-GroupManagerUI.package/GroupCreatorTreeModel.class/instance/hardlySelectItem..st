private
hardlySelectItem: anItem

	self selectNodePath: ((self setSelectedNodeItem: anItem)collect:#complexContents)
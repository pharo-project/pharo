row management
listChanged
	self changed: #list.
	self extent: self optimalExtent.
	self fitInWorld
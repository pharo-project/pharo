building widgets
clearToolbar

	self widgets removeAll.
	
	self needFullRebuild: true.
	self needRebuild: false.
	
 	self buildWithSpecLayout: self emptyLayout	

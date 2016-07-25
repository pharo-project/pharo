event-handling
onUnhovered
	(self stepModel isCategorySelected: self category)
		ifFalse: [ self hideActionbar ]
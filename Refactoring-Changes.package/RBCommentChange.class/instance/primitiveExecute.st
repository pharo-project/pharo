private
primitiveExecute
	self changeClass 
		classComment: comment
		stamp: self changeStamp.
	SystemAnnouncer uniqueInstance 
		classCommented: self changeClass
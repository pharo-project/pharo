instance creation
checkAllTags
	self assert: (self allSubclasses size = (self allSubclasses collect:#tag as:Set) size)
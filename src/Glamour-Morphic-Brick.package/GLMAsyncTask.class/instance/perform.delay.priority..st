async
perform: aBlock delay: anInteger priority: aPriority

	self
		execute: []
		withArguments: {}
		callback: aBlock
		priority: aPriority
		delay: anInteger
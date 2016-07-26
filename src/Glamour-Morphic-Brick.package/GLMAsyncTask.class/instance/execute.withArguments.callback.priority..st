async
execute: aBlock withArguments: aCollection callback: callbackBlock priority: aPriority
		
	^ self
		execute: aBlock
		withArguments: aCollection
		callback: callbackBlock
		priority: aPriority
		delay: 0
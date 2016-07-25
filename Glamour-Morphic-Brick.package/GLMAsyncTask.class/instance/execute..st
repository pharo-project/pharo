scripting
execute: anArguments

	^ self
		execute: self task
		withArguments: anArguments
		callback: self callback
		priority: self priority
		delay: self delay
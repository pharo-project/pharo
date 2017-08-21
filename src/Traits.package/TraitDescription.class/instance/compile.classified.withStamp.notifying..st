compiling
compile: text classified: category withStamp: changeStamp notifying: requestor
	^ self
		compile: text
		classified: category
		withStamp: changeStamp
		notifying: requestor
		logSource: self acceptsLoggingOfCompilation
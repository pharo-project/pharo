testing
isUpEnabled
	^ self argumentIndex ~= 0 and: [ self argumentIndex - 1 between: 1 and: self argumentList size ]
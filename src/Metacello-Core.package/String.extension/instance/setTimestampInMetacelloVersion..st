*metacello-core
setTimestampInMetacelloVersion: aMetacelloVersionSpec

	aMetacelloVersionSpec setTimestamp: 
		(aMetacelloVersionSpec project valueHolderSpec
			value: self;
			yourself)
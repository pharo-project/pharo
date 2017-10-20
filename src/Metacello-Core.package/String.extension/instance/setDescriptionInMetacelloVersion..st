*metacello-core
setDescriptionInMetacelloVersion: aMetacelloVersionSpec

	aMetacelloVersionSpec setDescription: 
		(aMetacelloVersionSpec project valueHolderSpec
			value: self;
			yourself)
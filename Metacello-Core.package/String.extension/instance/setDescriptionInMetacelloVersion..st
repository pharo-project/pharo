*Metacello-Core
setDescriptionInMetacelloVersion: aMetacelloVersionSpec

	aMetacelloVersionSpec setDescription: 
		(aMetacelloVersionSpec project valueHolderSpec
			value: self;
			yourself)
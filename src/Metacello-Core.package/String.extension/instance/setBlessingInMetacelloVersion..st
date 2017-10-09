*metacello-core
setBlessingInMetacelloVersion: aMetacelloVersionSpec

	aMetacelloVersionSpec setBlessing: 
		(aMetacelloVersionSpec project valueHolderSpec
			value: self;
			yourself)
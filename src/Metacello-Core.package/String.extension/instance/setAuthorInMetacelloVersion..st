*metacello-core
setAuthorInMetacelloVersion: aMetacelloVersionSpec

	aMetacelloVersionSpec setAuthor: 
		(aMetacelloVersionSpec project valueHolderSpec
			value: self;
			yourself)
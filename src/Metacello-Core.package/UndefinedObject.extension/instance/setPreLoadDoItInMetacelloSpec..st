*metacello-core
setPreLoadDoItInMetacelloSpec: aMetacelloSpec

	aMetacelloSpec setPreLoadDoIt: 
		(aMetacelloSpec project valueHolderSpec
			value: self;
			yourself)
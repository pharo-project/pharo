*Metacello-Core
setPreLoadDoItInMetacelloSpec: aMetacelloSpec

	aMetacelloSpec setPreLoadDoIt: 
		(aMetacelloSpec project valueHolderSpec
			value: self;
			yourself)
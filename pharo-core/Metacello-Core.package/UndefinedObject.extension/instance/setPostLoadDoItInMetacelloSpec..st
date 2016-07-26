*Metacello-Core
setPostLoadDoItInMetacelloSpec: aMetacelloSpec

	aMetacelloSpec setPostLoadDoIt: 
		(aMetacelloSpec project valueHolderSpec
			value: self;
			yourself)
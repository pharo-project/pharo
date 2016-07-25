*Metacello-Core
setTimestampInMetacelloVersion: aMetacelloVersionSpec

	aMetacelloVersionSpec setTimestamp: 
		(aMetacelloVersionSpec project valueHolderSpec
			value: self printString;
			yourself)
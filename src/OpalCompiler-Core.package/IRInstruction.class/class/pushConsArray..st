instance creation
pushConsArray: aSize 
	^IRPushArray new
		size: aSize;
		cons: true;
		yourself.
instance creation
pushNewArray: aSize
	
	^IRPushArray new
		size: aSize;
		cons: false;
		yourself.
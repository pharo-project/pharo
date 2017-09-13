initialization
initialize
	super initialize.
	self
		replace: '``@rcvr nextPutAll: ``@object1 , ``@object2' with: '``@rcvr nextPutAll: ``@object1; nextPutAll: ``@object2';
		replace: '``@rcvr show: ``@object1 , ``@object2' with: '``@rcvr show: ``@object1; show: ``@object2'
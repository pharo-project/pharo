running
testTearDownOrder
	| myResourceSetUpOrder myResourceReverseTearDownOrder |
	myResourceReverseTearDownOrder := OrderedCollection new: 7.
	myResourceSetUpOrder := (OrderedCollection new: 7)
		add: SimpleTestResource;
		add: SimpleTestResourceA1;
		add: SimpleTestResourceA2;
		add: SimpleTestResourceA;
		add: SimpleTestResourceB1;
		add: SimpleTestResourceB;
		yourself.
	self
		assert: (myResourceSetUpOrder allSatisfy: [:each | each isAvailable])
		description: 'At test start, not all my resources were set up'.
	self class resources
		do: [:each | each resetOrAddResourcesTo: myResourceReverseTearDownOrder].
	self
		assert: myResourceReverseTearDownOrder = myResourceSetUpOrder
		description: 'Wrong order for tearDown'.
	self
		assert: (myResourceSetUpOrder allSatisfy: [:each | each isAvailable])
		description: 'At test start, not all my resources were set up'
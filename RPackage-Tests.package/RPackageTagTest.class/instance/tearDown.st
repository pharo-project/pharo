running
tearDown 
	super tearDown.
	#(TestClass TestClassOther) 
		do: [ :each | each asClassIfPresent: #removeFromSystem ]
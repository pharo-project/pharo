running
tearDown 
	super tearDown.
	#(TestClass TestClassOther) 
		do: [ :each | 
			self class environment at: each ifPresent: #removeFromSystem ]
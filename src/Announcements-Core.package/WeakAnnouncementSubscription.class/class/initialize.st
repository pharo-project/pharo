class initialization
initialize
	"self initialize"
	
	
	WeakArray 
		removeWeakDependent: self;
		addWeakDependent: self
		
	"Make sure that it is not added twice."
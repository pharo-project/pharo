scope
newOptimizedBlockScope: int
	^ OCOptimizedBlockScope new 
			outerScope: self; 
			id: int;
			yourself.
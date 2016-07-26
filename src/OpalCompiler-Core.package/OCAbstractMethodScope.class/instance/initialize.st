initialization
initialize

	tempVars :=  OrderedDictionary new.
	tempVector  := Dictionary new.
	copiedVars := OCKeyedSet keyBlock: [:var | var name].
	id := 0.
	
	thisContextVar := OCThisContextVariable new
		scope: self; yourself.

tests
testNestedStructureWithArray
	| nestedStruct |	

	nestedStruct := FFITestNestingStructureWithArray new.
	self deny: nestedStruct nested getHandle isExternalAddress.
	self 
		assert: nestedStruct nested char5 asArray
		equals: {Character null. Character null. Character null. Character null. Character null }.
	self 
		assert: nestedStruct nested byte10 asArray 
		equals: #(0 0 0 0 0 0 0 0 0 0).
		
	nestedStruct nested char5: (FFITypeArray fromHandle: 'ABCDE' asByteArray).
	nestedStruct nested byte10: (FFITypeArray fromHandle: #[1 2 3 4 5 6 7 8 9 0]).
	self 
		assert: nestedStruct nested char5 asArray 
		equals: {$A. $B. $C. $D. $E}.
	self 
		assert: nestedStruct nested byte10 asArray 
		equals: #(1 2 3 4 5 6 7 8 9 0).
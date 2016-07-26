tests
testStructWithArray
	| structWithArrays |	

	structWithArrays := FFITestArrayStructure new.
	self deny: structWithArrays getHandle isExternalAddress.
	self 
		assert: structWithArrays char5 asArray
		equals: {Character null. Character null. Character null. Character null. Character null }.
	self 
		assert: structWithArrays byte10 asArray 
		equals: #(0 0 0 0 0 0 0 0 0 0).
		
	structWithArrays char5: (FFITypeArray fromHandle: 'ABCDE' asByteArray).
	structWithArrays byte10: (FFITypeArray fromHandle: #[1 2 3 4 5 6 7 8 9 0]).
	self 
		assert: structWithArrays char5 asArray 
		equals: {$A. $B. $C. $D. $E}.
	self 
		assert: structWithArrays byte10 asArray 
		equals: #(1 2 3 4 5 6 7 8 9 0).
	
	

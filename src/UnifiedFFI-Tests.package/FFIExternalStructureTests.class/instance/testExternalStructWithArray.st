tests
testExternalStructWithArray
	| structWithArrays structureSize |	

	structureSize := FFITestArrayStructure structureSize.
	structWithArrays := FFITestArrayStructure externalNew autoRelease.
	self assert: structWithArrays getHandle isExternalAddress.
	structWithArrays getHandle 
		replaceFrom: 1 to: structureSize with: (ByteArray new: structureSize). 
		
	structWithArrays char5: 'ABCDE'.
	structWithArrays byte10: #[1 2 3 4 5 6 7 8 9 0].
	self 
		assert: structWithArrays char5 asArray 
		equals: {$A. $B. $C. $D. $E}.
	self 
		assert: structWithArrays byte10 asArray 
		equals: #(1 2 3 4 5 6 7 8 9 0).
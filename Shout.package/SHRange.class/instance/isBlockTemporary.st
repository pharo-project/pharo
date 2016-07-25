tests
isBlockTemporary
	
	^type == #blockPatternTempVar or: [ type == #blockPatternArg ]
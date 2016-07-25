visiting
visitReturnNode: aReturnNode 
	codeStream
		nextPut: $^;
		nextPutAll: self stringFollowingReturn.
	self visitNode: aReturnNode value
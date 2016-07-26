testing
isMessageAGetter
	
	^ message numArgs = 0 and: [ class instVarNames includes: message selector ]
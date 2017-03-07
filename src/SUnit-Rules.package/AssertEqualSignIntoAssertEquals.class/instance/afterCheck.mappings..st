hooks
afterCheck: aNode mappings: mappingDict
	"would be nice to test if the class uses TAssertable,
	 but it is more complicated"
	
	^ [ aNode methodNode methodClass canUnderstand: #assert:equals: ]
		on: MessageNotUnderstood "methodNode or methodClass may be nil"
		do: [ false ]
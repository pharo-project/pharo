instance creation
parse: aStringOrStream onError: failBlock 
	^(self new)
		on: aStringOrStream;
		failBlock: failBlock;
		nextNumber
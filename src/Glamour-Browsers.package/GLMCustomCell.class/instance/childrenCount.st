accessing
childrenCount 
	 
	^self children 
		inject: 0 
		into: [:sum :each | sum + each span]
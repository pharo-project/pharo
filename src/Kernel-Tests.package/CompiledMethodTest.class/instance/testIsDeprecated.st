tests - testing
testIsDeprecated

	#(	
		deprecatedMethod 
	  	deprecatedMethod2 
	 	deprecatedMethod3 
		deprecatedMethod4
	) do: [ :each | 
		self assert: (self class >> each) isDeprecated 
	].
	
	self deny: (self class >> #testIsDeprecated) isDeprecated.

tests
testUnionWithDisjoint

	self assert: (aTimespan union: aDisjointTimespan)  = 
		(Timespan starting: jan01 duration: (14 days)).	
			

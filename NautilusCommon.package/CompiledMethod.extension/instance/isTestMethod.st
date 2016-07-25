*NautilusCommon
isTestMethod

    ^ self methodClass isTestCase and: [ 
		((self selector beginsWith: 'test') 
			or: [ (self selector beginsWith: 'should')]) and: [ self numArgs isZero ] ]
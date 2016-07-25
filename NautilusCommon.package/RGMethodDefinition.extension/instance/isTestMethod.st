*NautilusCommon
isTestMethod

    ^ (self methodClass inheritsFrom: TestCase) and: [ 
		((self selector beginsWith: 'test') 
			or: [ (self selector beginsWith: 'should')]) and: [ self numArgs isZero ] ]
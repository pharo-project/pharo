testing
isValid
	^ handle notNil and: [ handle anySatisfy: [ :each | each isZero not ] ]
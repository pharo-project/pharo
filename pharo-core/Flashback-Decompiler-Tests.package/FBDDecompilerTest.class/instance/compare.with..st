testing
compare: originalMethod with: newMethod
	| gp1 gp2 gp1Result gp2Result |
	gp1 := self exampleClass new. 
	gp2 := self exampleClass new.
	gp1Result := gp1 withArgs: #() executeMethod: newMethod.
	gp2Result := gp2 withArgs: #() executeMethod: originalMethod.
	self compareResult: gp1Result with: gp2Result.
	self comparePigs: gp1 with: gp2
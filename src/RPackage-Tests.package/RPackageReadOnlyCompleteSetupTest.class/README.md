The tests takes as fixture the following situation and exercises the readonly queries.
We should be able to use a test resources to speed it up.

P1 
	A1DefinedInP1
	A1DefinedInP1>>methodDefinedInP1
	B1DefinedInP1	
	A2DefinedInP2>>methodDefinedInP1
	
P2
	A2DefinedInP2
	A2DefinedInP2>>methodDefinedInP2
	B2DefinedInB2	

P3
	A3DefinedInP3	
	A2DefinedInP2>>methodDefinedInP3 
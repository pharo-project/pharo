The tests takes as fixture the following situation (which is constructed partially)
For tests that do not need to check the incremental construction.


P1 
	A1DefinedInP1>>methodDefinedInP1
	B1DefinedInP1	
	A2DefinedInP2>>methodDefinedInP1  (was *P2)
	
P2
	A2DefinedInP2>>methodDefinedInP2
	B2DefinedInB2	

P3
	A3DefinedInP3	
	A2DefinedInP2>>methodDefinedInP3  (was *P2*)
tests - testing
testContainsHalt
	
	| annonClass |
	
	self 
	deny: (Object >> #halt) containsHalt;
	deny: (Object >> #haltIfNil) containsHalt;
	deny: (Halt class >> #once) containsHalt;
	assert: (UndefinedObjectTest >> #testIfNil) containsHalt.
	
	annonClass := Object newAnonymousSubclass.
	annonClass 
	compile: 'm1 self halt';
	compile: 'm2 self haltIfNil';
	compile: 'm3 self yourself'.
	
	self
	assert: (annonClass >> #m1) containsHalt;
	assert: (annonClass >> #m2) containsHalt;
	deny: (annonClass >> #m3) containsHalt.
	
	
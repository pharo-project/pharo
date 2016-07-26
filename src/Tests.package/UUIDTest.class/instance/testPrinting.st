testing
testPrinting

	| s1 s2 s3 |
	
	s1 := '0608b9dc-02e4-4dd0-9f8a-ea45160df641'.
	s2 := 'e85ae7ba-3ca3-4bae-9f62-cc2ce51c525e'.
	s3 := '00000000-0000-0000-0000-000000000000'.
	
	self assert: (UUID fromString: s1) printString equals: s1.
	self assert: (UUID fromString: s2) printString equals: s2.
	self assert: UUID nilUUID printString equals: s3.
	
	self assert: (UUID fromString: s1) fullPrintString equals: ('an UUID(''', s1, ''')').
	self assert: (UUID fromString: s2) fullPrintString equals: ('an UUID(''', s2, ''')').
	self assert: UUID nilUUID fullPrintString equals: ('an UUID(''', s3, ''')').
	
	

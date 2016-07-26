tests
testReadFrom

	| s1 s2 s3 s4 s5 | 
	s1 := '2 June 1973' readStream.
	s2 := '2-JUN-73' readStream.
	s3 := 'June 2, 1973' readStream.
	s4 := '6/2/73' readStream.
	s5 := '2JUN73' readStream.

	self	
		assert: (self dateClass readFrom: s1) equals: june2nd1973;
		assert: (self dateClass readFrom: s2) equals: june2nd1973;
		assert: (self dateClass readFrom: s3) equals: june2nd1973;
		assert: (self dateClass readFrom: s4) equals: june2nd1973.
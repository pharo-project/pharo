tests
testFromSeconds

	| d |
	d := self dateClass fromSeconds: june2nd1973 asSeconds. 
	self assert: d equals: june2nd1973.

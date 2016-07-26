tests
testFromString
 	"The fromString method requires month, day and year delemited by a valid date delimiter"
	
	| d |
	d := self dateClass fromString: '06.02.1973'. 
	self assert: d equals: june2nd1973.
 
	d := self dateClass fromString: '06-02-1973'. 
	self assert: d equals: june2nd1973.

	d := self dateClass fromString: '06/02/1973'. 
	self assert: d equals: june2nd1973.	
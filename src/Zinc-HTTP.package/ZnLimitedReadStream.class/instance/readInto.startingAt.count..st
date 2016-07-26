accessing
readInto: collection startingAt: offset count: count 
	"Read count elements and place them in collection starting at offset.
	Return the number of elements actually read."
	
	| target actual |
	target := count min: (limit - position).
	actual := stream 
		readInto: collection 
		startingAt: offset 
		count: target.
	position := position + actual.
	^ actual
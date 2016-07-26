testing
testasAlien
	| alien alienValue |
	alien := Alien newCString: 'foo'.
	self should: [alien == alien asAlien].
	alien free.
	alienValue := false asAlien.
	self should: [alienValue isNumber and: [alienValue == 0]].
	alienValue := true asAlien.
	self should: [alienValue isNumber and: [alienValue == 1]].
	alienValue := nil asAlien.
	self should: [alienValue isNumber and: [alienValue == 0]].
tests
testCharacters
	| characters |
	characters := STON listClass withAll: ($a to: $z), ($A to: $Z).
	self serializeAndMaterialize: characters
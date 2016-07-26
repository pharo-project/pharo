testing
testShort
	| short |
	short := Alien new: 2.
	0 to: 65535 do: 
		[:i  | short unsignedShortAt: 1 put: i.
		self should: [(short unsignedShortAt: 1) = i]].
	-32768 to: 32767 do: 
		[:j |  short signedShortAt: 1 put: j.
		self should: [(short signedShortAt: 1) = j]].
	-65535 to: -32769 do: 
		[:k |  self should: [short signedShortAt: 1 put: k] raise: Error].
	32768 to: 65535 do: 
		[:k |  self should: [short signedShortAt: 1 put: k] raise: Error].
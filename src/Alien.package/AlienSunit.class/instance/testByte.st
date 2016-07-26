testing
testByte
	| byte |
	byte := Alien newC:1.
	0 to: 255 do: 
		[:i  | byte unsignedByteAt: 1 put: i.
		self should: [(byte unsignedByteAt: 1) = i]].
	-128 to: 127 do: 
		[:j |  byte signedByteAt: 1 put: j.
		self should: [(byte signedByteAt: 1) = j]].
	-255 to: -129 do: 
		[:k |  self should: [byte signedByteAt: 1 put: k] raise: Error].
	128 to: 255 do: 
		[:k |  self should: [byte signedByteAt: 1 put: k] raise: Error].
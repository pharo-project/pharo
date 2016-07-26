converting
fromInteger: address
	"set my handle to point at address."
	"Do we really need this? bf 2/21/2001 23:48"

	| sz pointer |
	sz := self class wordSize.
	pointer := ByteArray new: sz.
	pointer integerAt: 1 put: address size: sz signed: false. .
	self basicAt: 1 put: (pointer byteAt: 1);
		basicAt: 2 put: (pointer byteAt: 2);
		basicAt: 3 put: (pointer byteAt: 3);
		basicAt: 4 put: (pointer byteAt: 4).
	sz = 8 ifTrue:
		[self basicAt: 5 put: (pointer byteAt: 5);
			basicAt: 6 put: (pointer byteAt: 6);
			basicAt: 7 put: (pointer byteAt: 7);
			basicAt: 8 put: (pointer byteAt: 8)]
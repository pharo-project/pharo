instance creation
byte1: byte1 byte2: byte2 byte3: byte3 byte4: byte4 
	"Depending on high-order byte copy directly into a LargeInteger,
	or build up a SmallInteger by shifting"
	| value |
	byte4 < 16r40 ifTrue:
		[^ (byte4 bitShift: 24)
		 + (byte3 bitShift: 16)
		 + (byte2 bitShift: 8)
		 + byte1].
	value := LargePositiveInteger new: 4.
	value digitAt: 4 put: byte4.
	value digitAt: 3 put: byte3.
	value digitAt: 2 put: byte2.
	value digitAt: 1 put: byte1.
	^ value
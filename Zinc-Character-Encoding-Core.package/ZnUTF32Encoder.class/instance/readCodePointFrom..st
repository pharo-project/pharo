private
readCodePointFrom: stream
	| byte1 byte2 byte3 byte4 |
	byte1 := stream next.
	byte2 := stream next.
	byte3 := stream next.
	byte4 := stream next.
	(byte1 isNil or: [ byte2 isNil or: [ byte3 isNil or: [ byte4 isNil ] ] ])
		ifTrue: [ self errorIncomplete ].
	^ self isBigEndian
		ifTrue: [ 
			(byte1 bitShift: 24) + (byte2 bitShift: 16) + (byte3 bitShift: 8) + byte4 ] 
		ifFalse: [ 
			(byte4 bitShift: 24) + (byte3 bitShift: 16) + (byte2 bitShift: 8) + byte1 ]
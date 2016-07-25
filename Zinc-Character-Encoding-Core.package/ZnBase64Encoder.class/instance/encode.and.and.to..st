private
encode: byte1 and: byte2 and: byte3 to: stream
	stream nextPut: (self characterForValue: (byte1 bitShift: -2)).
	byte2
		ifNil: [ 
			stream nextPut: (self characterForValue: ((byte1 bitAnd: 2r11) bitShift: 4)).
			stream nextPutAll: '==' ]
		ifNotNil: [ 
			stream nextPut: (self characterForValue: (((byte1 bitAnd: 2r11) bitShift: 4) + (byte2 bitShift: -4))).
			byte3
				ifNil: [ 
					stream nextPut: (self characterForValue: ((byte2 bitAnd: 2r1111) bitShift: 2)).
					stream nextPut: $= ]
				ifNotNil: [ 
					stream nextPut: (self characterForValue: (((byte2 bitAnd: 2r1111) bitShift: 2) + (byte3 bitShift: -6))).
					stream nextPut: (self characterForValue: (byte3 bitAnd: 2r111111)) ] ]
private
decode: char1 and: char2 and: char3 and: char4 to: stream
	| v1 v2 v3 v4 |
	v1 := self valueForCharacter: char1.
	v2 := self valueForCharacter: char2.
	stream nextPut: (v1 bitShift: 2) + (v2 bitShift: -4).
	char3 == $=
		ifFalse: [ 
			v3 := self valueForCharacter: char3.
			stream nextPut: ((v2 bitAnd: 2r1111) bitShift: 4) + (v3 bitShift: -2).
			char4 == $=
				ifFalse: [ 
						v4 := self valueForCharacter: char4.
						stream nextPut: ((v3 bitAnd: 2r11) bitShift: 6) + v4 ] ]
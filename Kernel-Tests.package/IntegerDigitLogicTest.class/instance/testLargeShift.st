tests
testLargeShift
	"A sanity check for LargeInteger bitShifts"
	"self debug: #testLargeShift"
	| suite |
	suite := #(     "some numbers on 64 bits or less"
               '101101011101001100110111110110011101101101000001110110011'
               '1101101001100010011001101110100000111011011010100011101100'
               '101101101011110011001100110011011101011001111000100011101000'
               '10101101101000101001111111111100101101011001011000100011100000'
               '1000101010101001111011101010111001011111110011110001000110000000'
               '1100101010101000010011101000110010111110110011110000000000000001' ).
	"65 bits or less"
	suite := suite , (suite collect: [:e | '1' , e reversed ]).
	"129 bits or less"
	suite := suite , (suite collect: [:e | e ,e ]).
	suite do: [:bits | | num ls rs |
		num := Integer readFrom: bits readStream base: 2.
		0 to: bits size-1 do: [:shift |
			ls := (num bitShift: shift) printStringBase: 2.
			rs := (num bitShift: 0-shift) printStringBase: 2.
			self assert: ls = (bits , (String new: shift withAll: $0)).
			self assert: rs = (bits copyFrom: 1 to: bits size - shift).]]
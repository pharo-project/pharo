tests - bitLogic
testBitAt
	| trials bitSequence2 |

	self assert: (2r10 bitAt: 1) equals: 0.
	self assert: (2r10 bitAt: 2) equals: 1.
	
	self
		assert: ((1 to: 100) allSatisfy: [:i | (0 bitAt: i) = 0])
		description: 'all bits of zero are set to zero'.
	
	self
		assert: ((1 to: 100) allSatisfy: [:i | (-1 bitAt: i) = 1])
		description: 'In two complements, all bits of -1 are set to 1'.
		
	
	trials := #(
		'2r10010011'
		'2r11100100'
		'2r10000000'
		'2r0000101011011001'
		'2r1000101011011001'
		'2r0101010101011000'
		'2r0010011110110010'
		'2r0010011000000000'
		'2r00100111101100101000101011011001'
		'2r01110010011110110010100110101101'
		'2r10101011101011001010000010110110'
		'2r10101000000000000000000000000000'
		'2r0010101110101001110010100000101101100010011110110010100010101100'
		'2r1010101110101100101000001011011000100111101100101000101011011001'
		'2r1010101110101000000000000000000000000000000000000000000000000000').
	trials do: [:bitSequence | | aNumber |
		aNumber := Number readFrom: bitSequence.
		bitSequence2 := (bitSequence size - 2 to: 1 by: -1) inject: '2r' into: [:string :i | string copyWith: (Character digitValue: (aNumber bitAt: i))].
		self assert: bitSequence2 = bitSequence].
	
	trials do: [:bitSequence | | bitInvert |
		bitInvert := -1 - (Number readFrom: bitSequence).
		bitSequence2 := (bitSequence size - 2 to: 1 by: -1) inject: '2r' into: [:string :i | string copyWith: (Character digitValue: 1 - (bitInvert bitAt: i))].
		self assert: bitSequence2 = bitSequence description: '-1-x is similar to a bitInvert operation in two complement']
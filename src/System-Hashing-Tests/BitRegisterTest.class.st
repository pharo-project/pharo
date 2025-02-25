Class {
	#name : 'BitRegisterTest',
	#superclass : 'TestCase',
	#instVars : [
		'reg1',
		'reg2',
		'result'
	],
	#category : 'System-Hashing-Tests-Registers',
	#package : 'System-Hashing-Tests',
	#tag : 'Registers'
}

{ #category : 'tests' }
BitRegisterTest >> testSixtyFourBitRegisterAddition [

	reg1 := SixtyFourBitRegister new load: 0.
	reg2 := SixtyFourBitRegister new load: 1.
	reg1 += reg2.
	result := reg1 asInteger.
	
	self 
		assert: result 
		equals: 1.
]

{ #category : 'tests' }
BitRegisterTest >> testSixtyFourBitRegisterAdditionOverflow [

	| byteArray |
	byteArray := #[18 52 86 120].
	reg1 := ThirtyTwoBitRegister new loadFrom: byteArray at: 1.

	self assert: reg1 asInteger equals: 305419896
]

{ #category : 'tests' }
BitRegisterTest >> testSixtyFourBitRegisterAnd [

	reg1 := SixtyFourBitRegister new load: 16rFFFF0000.
	reg2 := SixtyFourBitRegister new load: 16r0000FFFF.
	reg1 bitAnd: reg2.
	result := reg1 asInteger.

	self assert: result equals: 0
]

{ #category : 'tests' }
BitRegisterTest >> testSixtyFourBitRegisterAsByteArray [

	| reg byteArray |
	reg := SixtyFourBitRegister new load: 1311768465173141112.

	byteArray := reg asByteArray.
	self
		assert: byteArray
		equals: #[ 18 52 86 120 18 52 86 120 ]
]

{ #category : 'tests' }
BitRegisterTest >> testSixtyFourBitRegisterByteAt [

    | byteArray register |
    byteArray := #[18 52 86 120 154 188 222 240].

    register := SixtyFourBitRegister new.
    register loadFrom: byteArray at: 1.

    "Check that the byteAt: method returns the expected values"
    self assert: (register byteAt: 1) equals: 16r12.
    self assert: (register byteAt: 2) equals: 16r34.
    self assert: (register byteAt: 3) equals: 16r56.
    self assert: (register byteAt: 4) equals: 16r78.
    self assert: (register byteAt: 5) equals: 16r9A.
    self assert: (register byteAt: 6) equals: 16rBC.
    self assert: (register byteAt: 7) equals: 16rDE.
    self assert: (register byteAt: 8) equals: 16rF0.

    "Check that byteAt: method returns nil for out-of-range indices"
    self should: [ register byteAt: 0 ] raise: Error.
    self should: [ register byteAt: 9 ] raise: Error.

]

{ #category : 'tests' }
BitRegisterTest >> testSixtyFourBitRegisterInversion [

	reg1 := SixtyFourBitRegister new load: 16rFFFFFFFF00000000.
	reg1 bitInvert.
	result := reg1 asInteger.

	self assert: result equals: 16r00000000FFFFFFFF
]

{ #category : 'tests' }
BitRegisterTest >> testSixtyFourBitRegisterLeftRotateBy2 [

	| initial rotated |
	
	initial := 1311768467463790320.
	rotated := 5247073869855161280. "initial left-rotated by 2 bits"
	
	reg1 := SixtyFourBitRegister new loadFrom: initial asByteArray at: 1.
	reg1 leftRotateBy: 2.

	self 
		assert: reg1 asInteger 
		equals: rotated.

]

{ #category : 'tests' }
BitRegisterTest >> testSixtyFourBitRegisterLeftRotateBy3 [

	| initial rotated |
	
	initial := 1311768467463790320.
	rotated := 10494147739710322560.		
	reg1 := SixtyFourBitRegister new loadFrom: initial asByteArray at: 1.
	reg1 leftRotateBy: 3.

	self 
		assert: reg1 asInteger 
		equals: rotated.


]

{ #category : 'tests' }
BitRegisterTest >> testSixtyFourBitRegisterLeftRotateBySimple [

	| byteArray initial rotated |

	initial := 2r11.
	rotated := 2r1100. "Left-rotated by 2 bits"

	byteArray := #[ 0 0 0 0 0 0 0 3 ].
	reg1 := SixtyFourBitRegister new loadFrom: byteArray at: 1.
	reg1 leftRotateBy: 2.

	self 
		assert: reg1 asInteger 
		equals: rotated.
]

{ #category : 'tests' }
BitRegisterTest >> testSixtyFourBitRegisterLoadFromCorrectByteOrder [

	| byteArray |
	byteArray := #[18 52 86 120 154 188 222 240].
	reg1 := SixtyFourBitRegister new loadFrom: byteArray at: 1.

	self assert: reg1 asInteger equals: 1311768467463790320
]

{ #category : 'tests' }
BitRegisterTest >> testSixtyFourBitRegisterLoadFromReverseByteOrder [

	| byteArray |
	byteArray := #[240 222 188 154 120 86 52 18].
	reg1 := SixtyFourBitRegister new reverseLoadFrom: byteArray at: 1.

	self assert: reg1 asInteger equals: 1311768467463790320
]

{ #category : 'tests' }
BitRegisterTest >> testSixtyFourBitRegisterOr [

	reg1 := SixtyFourBitRegister new load: 16rFFFFFFFF00000000.
	reg2 := SixtyFourBitRegister new load: 16r00000000FFFFFFFF.
	reg1 bitOr: reg2.
	result := reg1 asInteger.

	self assert: result equals: 16rFFFFFFFFFFFFFFFF
]

{ #category : 'tests' }
BitRegisterTest >> testSixtyFourBitRegisterXor [

	reg1 := SixtyFourBitRegister new load: 16rFFFFFFFF00000000.
	reg2 := SixtyFourBitRegister new load: 16r00000000FFFFFFFF.
	reg1 bitXor: reg2.
	result := reg1 asInteger.

	self assert: result equals: 16rFFFFFFFFFFFFFFFF
]

{ #category : 'tests' }
BitRegisterTest >> testThirtyTwoBitRegisterAddition [

	reg1 := ThirtyTwoBitRegister new load: 0.
	reg2 := ThirtyTwoBitRegister new load: 1.
	reg1 += reg2.
	result := reg1 asInteger.
	
	self 
		assert: result 
		equals: 1.
]

{ #category : 'tests' }
BitRegisterTest >> testThirtyTwoBitRegisterAdditionOverflow [

	reg1 := ThirtyTwoBitRegister new load: 16rFFFFFFFF.
	reg2 := ThirtyTwoBitRegister new load: 1.
	reg1 += reg2.
	result := reg1 asInteger.
	
	self 
		assert: result 
		equals: 0.
]

{ #category : 'tests' }
BitRegisterTest >> testThirtyTwoBitRegisterAnd [

	reg1 := ThirtyTwoBitRegister new load: 16rFFFF0000.
	reg2 := ThirtyTwoBitRegister new load: 16r0000FFFF.
	reg1 bitAnd: reg2.
	result := reg1 asInteger.

	self assert: result equals: 0
]

{ #category : 'tests' }
BitRegisterTest >> testThirtyTwoBitRegisterInversion [

	reg1 := ThirtyTwoBitRegister new load: 16rFFFF0000.
	reg1 bitInvert.
	result := reg1 asInteger.

	self assert: result equals: 16r0000FFFF
]

{ #category : 'tests' }
BitRegisterTest >> testThirtyTwoBitRegisterLoadFromCorrectByteOrder [

	| byteArray |
	byteArray := #[18 52 86 120].
	reg1 := ThirtyTwoBitRegister new loadFrom: byteArray at: 1.

	self assert: reg1 asInteger equals: 305419896
]

{ #category : 'tests' }
BitRegisterTest >> testThirtyTwoBitRegisterLoadFromReverseByteOrder [

	| byteArray |
	byteArray := #[120 86 52 18].
	reg1 := ThirtyTwoBitRegister new reverseLoadFrom: byteArray at: 1.

	self assert: reg1 asInteger equals: 305419896
]

{ #category : 'tests' }
BitRegisterTest >> testThirtyTwoBitRegisterOr [

	reg1 := ThirtyTwoBitRegister new load: 16rFFFF0000.
	reg2 := ThirtyTwoBitRegister new load: 16r0000FFFF.
	reg1 bitOr: reg2.
	result := reg1 asInteger.

	self assert: result equals: 16rFFFFFFFF
]

{ #category : 'tests' }
BitRegisterTest >> testThirtyTwoBitRegisterXor [

	reg1 := ThirtyTwoBitRegister new load: 16rFFFF0000.
	reg2 := ThirtyTwoBitRegister new load: 16r0000FFFF.
	reg1 bitXor: reg2.
	result := reg1 asInteger.

	self assert: result equals: 16rFFFFFFFF
]

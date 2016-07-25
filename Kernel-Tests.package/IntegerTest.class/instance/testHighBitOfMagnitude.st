tests - bitLogic
testHighBitOfMagnitude
	| suite |

	suite := (0 to: 1024) asArray , #(16rFDFD 16rFFFF 16r1000 16r1000000 16r1000001 16r70000000 16r7AFAFAFA ) , {SmallInteger maxVal . SmallInteger maxVal+1}.
	suite := suite , (suite collect: [:e | e raisedTo: 20]).
	
	suite do: [:anInteger |
		| highBit shifted |
		highBit := 0.
		shifted := 1.
		[shifted > anInteger] whileFalse: [highBit := highBit+1. shifted := shifted bitShift: 1].
		self assert: anInteger highBitOfMagnitude = highBit.
		self assert: anInteger negated highBitOfMagnitude = highBit].
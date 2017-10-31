special literal encodings
isSpecialLiteralForPush: literal
	^literal == false
	  or: [literal == true
	  or: [literal == nil
	  or: [(literal isInteger and: [literal between: -32768 and: 32767])
	  or: [(literal isCharacter and: [literal asInteger between: 0 and: 65535])]]]]
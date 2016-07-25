converting
nextPutCodePoint: codePoint toStream: stream
	"In non-strict mode, we let code points for holes in our mapping table pass through"
	
	codePoint < 128
		ifTrue: [ stream nextPut: codePoint ]
		ifFalse: [ 
			| byte |
			byte := unicodeToByte at: codePoint ifAbsent: [ nil ].
			(byte isNil and: [ strict or: [ codePoint > 255 ] ])
				ifTrue: [ self errorOutsideRange ].
			stream nextPut: (byte ifNil: [ codePoint ]) ]
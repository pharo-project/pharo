compiled method support
extensionsAt: bcpc in: method into: aTrinaryBlock
	"If the bytecode at pc is an extension then evaluate aBinaryBlock with the values of extA and extB and number of extension *bytes*.
	 If the bytecode at pc is not extended then evaluate aBinaryBlock with 0 and 0.
	224		11100000	aaaaaaaa	Extend A (Ext A = Ext A prev * 256 + Ext A)
	225		11100001	bbbbbbbb	Extend B (Ext B = Ext B prev * 256 + Ext B)"
 
	| scanpc byte extByte extA extB |
	scanpc := bcpc.
	"There may be an extension (it could be a false positive).  We must scan as fast as possible..."
	extA := extB := 0.
	[byte := method at: scanpc.
	 byte >= 224 and: [byte <= 225]] whileTrue: 
		[extByte := method at: scanpc + 1.
		 scanpc := scanpc + 2.
		 byte = 224
			ifTrue:
				[extA := (extA bitShift: 8) + extByte]
			ifFalse:
				[extB := (extB = 0 and: [extByte > 127])
					ifTrue: [extByte - 256]
					ifFalse: [(extB bitShift: 8) + extByte]]].
	^aTrinaryBlock value: extA value: extB value: scanpc - bcpc


"Why use
	byte >= 224 and: [byte <= 225]
 and not
	(byte bitAnd: 16rFE) = 16rE0
 ?
 | n |
 n := 100000000.
 #(0 224) collect:
	[:byte|
	{ Time millisecondsToRun: [1 to: n do: [:i| (byte >= 224 and: [byte <= 225]) ifTrue: []]].
	   Time millisecondsToRun: [1 to: n do: [:i| (byte bitAnd: 16rFE) = 16rE0 ifTrue: []]] }] #(#(297 599) #(702 671))"
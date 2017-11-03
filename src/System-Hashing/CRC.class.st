"
Parameterized implementation of the cyclic redundancy check (CRC) algorithm.

INTRODUCTION
=================
This implementation is based on the (awesome) paper ""A Painless Guide to CRC Error Detection Algorithms"" by Ross Williams. You should find a copy of the paper here: http://www.ross.net/crc/. In this paper Ross describes a parameterized implementation that enables the different variations of the CRC algorithm to be used in a consistent way, simply by adjusting the parameters. If you don't have a clue about CRC (like me) then I strongly suggest reading the paper. It will also help you to understand how to make the best use of this implementation.

The ""CRC RevEng"" project on sourceforge implements Williams's ""RockSoft"" parameterized CRC program (as does this class) and comes with a handy list of parameters for various CRC algorithm: http://reveng.sourceforge.net/crc-catalogue/.

For ease of use and better performance, the two defacto standard variations ""CRC16"" and ""CRC32"" have been predefined. The lookup tables for these implementations are included on the class side. For all other variations the lookup table will be generated at runtime before the first run.

If you want to define your own algorithm you can do so by using the methods in the ""accessing-parameters"" protocol. Note that there are no default values. Here's a short overview:
	#width: 			defines the width of the register (usually 16 or 32)
	#polynome: 		defines the polynome to use for polynome division / lookup table creation
	#registerFill: 		defines the start content of the working register (usually all ones or all zeros)
	#reflectInput: 		if true every byte will be reflected before processing (e.g. 100101 -> 101001)
	#reflectOutpu: 		if true the entire register will be reflected before the final XOR stage
	#finallyXorWith: 	defines the final XOR for the entire register
	#lookupTable: 		the only OPTIONAL parameter. The lookup table will be generated at runtime if none has been supplied
	#message: 			the message to calculate the CRC on 
		

EXAMPLES
=================
The simplest possible snippet uses the class side methods for ""CRC16"" and ""CRC32"":
	CRC crc16FromCollection: 'some message'. --> 55709
	CRC crc32FromCollection: 'some message'. --> 191312361

Let's assume, you wanted to use ""CRC16 reversed"" (neither input nor output reflected). Then you would have to change the parameters like so (the reversed form uses a different polynome and a different start register content):
	crc := CRC new
		beCrc16;
		polynome: 16r1021;
		registerFill: 16rFFFF;
		reflectInput: false;
		reflectOutput: false;
		message: 'some message';
		yourself.
	crc run. --> 46785
	
Using a single instance as in the code above will of course be faster than using the class side methods when performing multiple runs. But if you are really concerned about performance (see PERFORMANCE) you should use the ""raw"" methods (no checks! If you forget to set parameters there will be errors....):
	crc := CRC new
		beCrc16;
		message: 'some message';
		yourself.
	crc runRefInRefOut. --> 55709
	
	crc := CRC new
		beCrc16;
		polynome: 16r1021;
		registerFill: 16rFFFF;
		message: 'some message';
		yourself.
	crc runNonRefInNonRefOut. --> 46785
	

PERFORMANCE
=================
The performance of this implementation (tested for crc16) is equal to the performance of String>>crc16 if executed ""raw"" (see EXAMPLES). For the users sake however, the implementation does a few extra checks to improve ease of use. The cost is a loss of performance of about factor 1.15 (single instance) and 1.42 (one instance per run) (note that although I took an average of 10, the results will vary quite a bit each time you run the code):
	crc := CRC new 
		beCrc16; 
		message: 'this is a test message'; 
		yourself.
	
	""String>>crc16""	
	times := OrderedCollection new.
	10 timesRepeat: [ times add: [ 1000000 timesRepeat: [ 'this is a test message' crc16 ] ] timeToRun ].
	times average floor. --> 530
	
	""raw""
	times := OrderedCollection new.
	10 timesRepeat: [ times add: [ 1000000 timesRepeat: [ crc runRefInRefOut ] ] timeToRun ].
	times average floor. --> 535
	
	""user friendly, one instance""	
	times := OrderedCollection new.
	10 timesRepeat: [ times add: [ 1000000 timesRepeat: [ crc run ] ] timeToRun ].
	times average floor. --> 616
	
	""user friendly, one instance per run""
	times := OrderedCollection new.
	10 timesRepeat: [ times add: [ 1000000 timesRepeat: [ CRC crc16FromCollection: 'this is a test message' ] ] timeToRun ].
	times average floor. --> 759
"
Class {
	#name : #CRC,
	#superclass : #Checksum,
	#instVars : [
		'width',
		'polynome',
		'registerFill',
		'finalXorBytes',
		'shouldReflectOutput',
		'lookupTable',
		'widthMask',
		'theRegister',
		'shouldReflectInput',
		'message',
		'runMethodSelector',
		'lowestByteShift'
	],
	#category : #System-Hashing-Checksums
}

{ #category : #'instance creation' }
CRC class >> crc16FromCollection: aCollection [
	| instance |
	instance := 
		self new
			beCrc16;
			message: aCollection;
			yourself.
			
	^ instance runRefInRefOut
]

{ #category : #accessing-tables }
CRC class >> crc16Table [
	^ #(	16r0000	16rC0C1	16rC181	16r0140	16rC301	16r03C0	16r0280	16rC241
			16rC601	16r06C0	16r0780	16rC741	16r0500	16rC5C1	16rC481	16r0440
			16rCC01	16r0CC0	16r0D80	16rCD41	16r0F00	16rCFC1	16rCE81	16r0E40
			16r0A00	16rCAC1	16rCB81	16r0B40	16rC901	16r09C0	16r0880	16rC841
			16rD801	16r18C0	16r1980	16rD941	16r1B00	16rDBC1	16rDA81	16r1A40
			16r1E00	16rDEC1	16rDF81	16r1F40	16rDD01	16r1DC0	16r1C80	16rDC41
			16r1400	16rD4C1	16rD581	16r1540	16rD701	16r17C0	16r1680	16rD641
			16rD201	16r12C0	16r1380	16rD341	16r1100	16rD1C1	16rD081	16r1040
			16rF001	16r30C0	16r3180	16rF141	16r3300	16rF3C1	16rF281	16r3240
			16r3600	16rF6C1	16rF781	16r3740	16rF501	16r35C0	16r3480	16rF441
			16r3C00	16rFCC1	16rFD81	16r3D40	16rFF01	16r3FC0	16r3E80	16rFE41
			16rFA01	16r3AC0	16r3B80	16rFB41	16r3900	16rF9C1	16rF881	16r3840
			16r2800	16rE8C1	16rE981	16r2940	16rEB01	16r2BC0	16r2A80	16rEA41
			16rEE01	16r2EC0	16r2F80	16rEF41	16r2D00	16rEDC1	16rEC81	16r2C40
			16rE401	16r24C0	16r2580	16rE541	16r2700	16rE7C1	16rE681	16r2640
			16r2200	16rE2C1	16rE381	16r2340	16rE101	16r21C0	16r2080	16rE041
			16rA001	16r60C0	16r6180	16rA141	16r6300	16rA3C1	16rA281	16r6240
			16r6600	16rA6C1	16rA781	16r6740	16rA501	16r65C0	16r6480	16rA441
			16r6C00	16rACC1	16rAD81	16r6D40	16rAF01	16r6FC0	16r6E80	16rAE41
			16rAA01	16r6AC0	16r6B80	16rAB41	16r6900	16rA9C1	16rA881	16r6840
			16r7800	16rB8C1	16rB981	16r7940	16rBB01	16r7BC0	16r7A80	16rBA41
			16rBE01	16r7EC0	16r7F80	16rBF41	16r7D00	16rBDC1	16rBC81	16r7C40
			16rB401	16r74C0	16r7580	16rB541	16r7700	16rB7C1	16rB681	16r7640
			16r7200	16rB2C1	16rB381	16r7340	16rB101	16r71C0	16r7080	16rB041
			16r5000	16r90C1	16r9181	16r5140	16r9301	16r53C0	16r5280	16r9241
			16r9601	16r56C0	16r5780	16r9741	16r5500	16r95C1	16r9481	16r5440
			16r9C01	16r5CC0	16r5D80	16r9D41	16r5F00	16r9FC1	16r9E81	16r5E40
			16r5A00	16r9AC1	16r9B81	16r5B40	16r9901	16r59C0	16r5880	16r9841
			16r8801	16r48C0	16r4980	16r8941	16r4B00	16r8BC1	16r8A81	16r4A40
			16r4E00	16r8EC1	16r8F81	16r4F40	16r8D01	16r4DC0	16r4C80	16r8C41
			16r4400	16r84C1	16r8581	16r4540	16r8701	16r47C0	16r4680	16r8641
			16r8201	16r42C0	16r4380	16r8341	16r4100	16r81C1	16r8081	16r4040)
]

{ #category : #'instance creation' }
CRC class >> crc32FromCollection: aCollection [
	| instance |
	instance := 
		self new
			beCrc32;
			message: aCollection;
			yourself.
	
	^ instance runRefInRefOut
]

{ #category : #accessing-tables }
CRC class >> crc32Table [
	^ #(	 16r00000000	 16r77073096	 16rEE0E612C	 16r990951BA	
			 16r076DC419	 16r706AF48F	 16rE963A535	 16r9E6495A3	
			 16r0EDB8832	 16r79DCB8A4	 16rE0D5E91E	 16r97D2D988	
			 16r09B64C2B	 16r7EB17CBD	 16rE7B82D07	 16r90BF1D91	
			 16r1DB71064	 16r6AB020F2	 16rF3B97148	 16r84BE41DE	
			 16r1ADAD47D	 16r6DDDE4EB	 16rF4D4B551	 16r83D385C7	
			 16r136C9856	 16r646BA8C0	 16rFD62F97A	 16r8A65C9EC	
			 16r14015C4F	 16r63066CD9	 16rFA0F3D63	 16r8D080DF5	
			 16r3B6E20C8	 16r4C69105E	 16rD56041E4	 16rA2677172	
			 16r3C03E4D1	 16r4B04D447	 16rD20D85FD	 16rA50AB56B	
			 16r35B5A8FA	 16r42B2986C	 16rDBBBC9D6	 16rACBCF940	
			 16r32D86CE3	 16r45DF5C75	 16rDCD60DCF	 16rABD13D59	
			 16r26D930AC	 16r51DE003A	 16rC8D75180	 16rBFD06116	
			 16r21B4F4B5	 16r56B3C423	 16rCFBA9599	 16rB8BDA50F	
			 16r2802B89E	 16r5F058808	 16rC60CD9B2	 16rB10BE924	
			 16r2F6F7C87	 16r58684C11	 16rC1611DAB	 16rB6662D3D	
			 16r76DC4190	 16r01DB7106	 16r98D220BC	 16rEFD5102A	
			 16r71B18589	 16r06B6B51F	 16r9FBFE4A5	 16rE8B8D433	
			 16r7807C9A2	 16r0F00F934	 16r9609A88E	 16rE10E9818	
			 16r7F6A0DBB	 16r086D3D2D	 16r91646C97	 16rE6635C01	
			 16r6B6B51F4	 16r1C6C6162	 16r856530D8	 16rF262004E	
			 16r6C0695ED	 16r1B01A57B	 16r8208F4C1	 16rF50FC457	
			 16r65B0D9C6	 16r12B7E950	 16r8BBEB8EA	 16rFCB9887C	
			 16r62DD1DDF	 16r15DA2D49	 16r8CD37CF3	 16rFBD44C65	
			 16r4DB26158	 16r3AB551CE	 16rA3BC0074	 16rD4BB30E2	
			 16r4ADFA541	 16r3DD895D7	 16rA4D1C46D	 16rD3D6F4FB	
			 16r4369E96A	 16r346ED9FC	 16rAD678846	 16rDA60B8D0	
			 16r44042D73	 16r33031DE5	 16rAA0A4C5F	 16rDD0D7CC9	
			 16r5005713C	 16r270241AA	 16rBE0B1010	 16rC90C2086	
			 16r5768B525	 16r206F85B3	 16rB966D409	 16rCE61E49F	
			 16r5EDEF90E	 16r29D9C998	 16rB0D09822	 16rC7D7A8B4	
			 16r59B33D17	 16r2EB40D81	 16rB7BD5C3B	 16rC0BA6CAD	
			 16rEDB88320	 16r9ABFB3B6	 16r03B6E20C	 16r74B1D29A	
			 16rEAD54739	 16r9DD277AF	 16r04DB2615	 16r73DC1683	
			 16rE3630B12	 16r94643B84	 16r0D6D6A3E	 16r7A6A5AA8	
			 16rE40ECF0B	 16r9309FF9D	 16r0A00AE27	 16r7D079EB1	
			 16rF00F9344	 16r8708A3D2	 16r1E01F268	 16r6906C2FE	
			 16rF762575D	 16r806567CB	 16r196C3671	 16r6E6B06E7	
			 16rFED41B76	 16r89D32BE0	 16r10DA7A5A	 16r67DD4ACC	
			 16rF9B9DF6F	 16r8EBEEFF9	 16r17B7BE43	 16r60B08ED5	
			 16rD6D6A3E8	 16rA1D1937E	 16r38D8C2C4	 16r4FDFF252	
			 16rD1BB67F1	 16rA6BC5767	 16r3FB506DD	 16r48B2364B	
			 16rD80D2BDA	 16rAF0A1B4C	 16r36034AF6	 16r41047A60	
			 16rDF60EFC3	 16rA867DF55	 16r316E8EEF	 16r4669BE79	
			 16rCB61B38C	 16rBC66831A	 16r256FD2A0	 16r5268E236	
			 16rCC0C7795	 16rBB0B4703	 16r220216B9	 16r5505262F	
			 16rC5BA3BBE	 16rB2BD0B28	 16r2BB45A92	 16r5CB36A04	
			 16rC2D7FFA7	 16rB5D0CF31	 16r2CD99E8B	 16r5BDEAE1D	
			 16r9B64C2B0	 16rEC63F226	 16r756AA39C	 16r026D930A	
			 16r9C0906A9	 16rEB0E363F	 16r72076785	 16r05005713	
			 16r95BF4A82	 16rE2B87A14	 16r7BB12BAE	 16r0CB61B38	
			 16r92D28E9B	 16rE5D5BE0D	 16r7CDCEFB7	 16r0BDBDF21	
			 16r86D3D2D4	 16rF1D4E242	 16r68DDB3F8	 16r1FDA836E	
			 16r81BE16CD	 16rF6B9265B	 16r6FB077E1	 16r18B74777	
			 16r88085AE6	 16rFF0F6A70	 16r66063BCA	 16r11010B5C	
			 16r8F659EFF	 16rF862AE69	 16r616BFFD3	 16r166CCF45	
			 16rA00AE278	 16rD70DD2EE	 16r4E048354	 16r3903B3C2	
			 16rA7672661	 16rD06016F7	 16r4969474D	 16r3E6E77DB	
			 16rAED16A4A	 16rD9D65ADC	 16r40DF0B66	 16r37D83BF0	
			 16rA9BCAE53	 16rDEBB9EC5	 16r47B2CF7F	 16r30B5FFE9	
			 16rBDBDF21C	 16rCABAC28A	 16r53B39330	 16r24B4A3A6	
			 16rBAD03605	 16rCDD70693	 16r54DE5729	 16r23D967BF	
			 16rB3667A2E	 16rC4614AB8	 16r5D681B02	 16r2A6F2B94	
			 16rB40BBE37	 16rC30C8EA1	 16r5A05DF1B	 16r2D02EF8D)
]

{ #category : #primitives }
CRC class >> update: oldCrc from: start to: stop in: aCollection [
	| newCrc |
	<primitive: 'primitiveUpdateGZipCrc32' module: 'ZipPlugin'>
	newCrc := oldCrc.
	start to: stop do: [ :i |
		newCrc := (self crc32Table at: ((newCrc bitXor: (aCollection byteAt: i)) 
				bitAnd: 255) + 1) bitXor: (newCrc bitShift: -8) ].
	^newCrc
]

{ #category : #accessing-implementations }
CRC >> beCrc16 [
	self
		width: 16;
		lookupTable: self class crc16Table;
		polynome: 16r8005;
		registerFill: 16r0;
		reflectInput: true;
		reflectOutput: true;
		finallyXorWith: 16r0
]

{ #category : #accessing-implementations }
CRC >> beCrc32 [
	self
		width: 32;
		lookupTable: self class crc32Table;
		polynome: 16r04C11DB7;
		registerFill: 16rFFFFFFFF;
		reflectInput: true;
		reflectOutput: true;
		finallyXorWith: 16rFFFFFFFF
]

{ #category : #private-bit-manipulation }
CRC >> bitMaskAt: anInteger [
	^ 0 bitAt: anInteger put: 1
]

{ #category : #accessing-parameters }
CRC >> finallyXorWith: anInteger [
	"The final XOR of the output will be performed with this value"
	finalXorBytes := anInteger
]

{ #category : #private-tables }
CRC >> generateLookupTable [
	"lookup tables have 256 entries"
	^ Array 
		new: 256
		streamContents: [ :aStream | self printLookupTableOn: aStream ]
]

{ #category : #private-bit-manipulation }
CRC >> invertedBitMaskAt: anInteger [
	anInteger < 1 ifTrue: [ ^ 16rFFFFFFFF ].
	^ 16rFFFFFFFF bitAt: anInteger put: 0
]

{ #category : #accessing-parameters }
CRC >> lookupTable: anArray [
	lookupTable := anArray
]

{ #category : #private-bit-manipulation }
CRC >> lowestByteShift [
	^ lowestByteShift ifNil: [ lowestByteShift := width - 8 ]
]

{ #category : #accessing-parameters }
CRC >> message: anObject [
	message := anObject asByteArray
]

{ #category : #accessing-parameters }
CRC >> polynome: anInteger [
	"The polynome used for polynomial division.
	It should be of the same length as the register width (see #width:)."
	polynome := anInteger
]

{ #category : #private-tables }
CRC >> printLookupTableOn: aStream [
	| topBit |
	topBit := self bitMaskAt: width.
	0 to: 255 do: [ :index || register indexByte |
		indexByte := index.
		shouldReflectInput ifTrue: [ indexByte := self reflect: indexByte onLowerBits: 8 ].
		register := indexByte << self lowestByteShift.
		1 to: 8 do: [ : byteIndex |
			register :=
				(register bitAnd: topBit) > 0 
					ifTrue: [ (register << 1) bitXor: polynome ]
					ifFalse: [ register << 1 ] ].
		shouldReflectInput ifTrue: [ 
			register := self reflect: register onLowerBits: width ].
		register := (register bitAnd: self widthMask).
		aStream nextPut: register ]
]

{ #category : #private-bit-manipulation }
CRC >> reflect: anInteger onLowerBits: anotherInteger [
	| register test |
	register := anInteger.
	test := anInteger.
	0 to: anotherInteger - 1 do: [ :index |
		register := 
			(test bitAnd: 1) = 1
				ifTrue: [ register bitOr: (self bitMaskAt: anotherInteger - index) ]
				ifFalse: [ register bitAnd: (self invertedBitMaskAt: anotherInteger - index) ].
	test := test bitShift: -1 ].

	^ register
]

{ #category : #accessing-parameters }
CRC >> reflectInput: aBoolean [
	"Determines if each byte should be reflected before processing.
	If false, bit 7 will be treated as most significant bit.
	If true, each byte will be reflected (bit 0 will be most significant)."
	shouldReflectInput := aBoolean
]

{ #category : #accessing-parameters }
CRC >> reflectOutput: aBoolean [
	"Determines if the output is reflected before the final XOR."
	shouldReflectOutput := aBoolean
]

{ #category : #accessing-parameters }
CRC >> registerFill: anInteger [
	"The initial value of the CRC register"
	registerFill := anInteger
]

{ #category : #public }
CRC >> run [
	lookupTable ifNil: [ lookupTable := self generateLookupTable ].
	^ self perform: self runMethodSelector
]

{ #category : #private-run-methods }
CRC >> runMethodSelector [
	^ runMethodSelector ifNil: [ 
		runMethodSelector := 
			shouldReflectInput
				ifTrue: [ 
					shouldReflectOutput
						ifTrue: [ #runRefInRefOut ] 
						ifFalse: [ #runRefInNonRefOut ] ]
				ifFalse: [ 
					shouldReflectOutput 
						ifTrue: [ #runNonRefInRefOut ] 
						ifFalse: [ #runNonRefInNonRefOut ] ] ]
]

{ #category : #private-run-methods }
CRC >> runNonRefInNonRefOut [
	theRegister := registerFill.
	1 to: message size do: [ :byteIndex | 
		theRegister := 
			(lookupTable at: (((theRegister bitShift: 0 - self lowestByteShift) bitXor: (message byteAt: byteIndex)) bitAnd: 255) + 1)
			bitXor: ((theRegister bitShift: 8) bitAnd: self widthMask) ].
			
	^ finalXorBytes bitXor: theRegister
]

{ #category : #private-run-methods }
CRC >> runNonRefInRefOut [
	Warning signal: 'unverified implementation'.
	theRegister := registerFill.
	1 to: message size do: [ :byteIndex | 
		theRegister := 
			(lookupTable at: (((theRegister bitShift: 0 - self lowestByteShift) bitXor: (message byteAt: byteIndex)) bitAnd: 255) + 1)
			bitXor: (theRegister bitShift: -8) ].
			
	^ finalXorBytes bitXor: theRegister
]

{ #category : #private-run-methods }
CRC >> runRefInNonRefOut [
	Warning signal: 'unverified implementation'.
	theRegister := registerFill.
	1 to: message size do: [ :byteIndex | 
		theRegister := 
			(lookupTable at: ((theRegister bitXor: (message byteAt: byteIndex)) bitAnd: 255) + 1) 
			bitXor: ((theRegister bitShift: 8) bitAnd: self widthMask) ].
			
	^ finalXorBytes bitXor: theRegister
]

{ #category : #private-run-methods }
CRC >> runRefInRefOut [
	theRegister := registerFill.
	1 to: message size do: [ :byteIndex | 
		theRegister := 
			(lookupTable at: ((theRegister bitXor: (message byteAt: byteIndex)) bitAnd: 255) + 1) 
			bitXor: (theRegister bitShift: -8) ].
			
	^ finalXorBytes bitXor: theRegister
]

{ #category : #accessing-parameters }
CRC >> width: anInteger [
	"width of the register (on how many bits the CRC is calculated).
	usually 16 or 32"
	width := anInteger
]

{ #category : #private-bit-manipulation }
CRC >> widthMask [ 	
	"bit mask (all ones)"
	^ widthMask ifNil: [ widthMask := (2 raisedTo: width) - 1 ]
]

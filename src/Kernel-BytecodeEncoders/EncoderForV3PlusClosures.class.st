"
An encoder for the V3 bytecode set augmented with the following bytecodes that are part of the full closure implementation.
	138   10001010 jkkkkkkk		Push (Array new: kkkkkkk) (j = 0)
								or	Pop kkkkkkk elements into: (Array new: kkkkkkk) (j = 1)

	140   10001100 kkkkkkkk jjjjjjjj 	Push Temp At kkkkkkkk In Temp Vector At: jjjjjjjj
	141   10001101 kkkkkkkk jjjjjjjj 	Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj
	142   10001110 kkkkkkkk jjjjjjjj 	Pop and Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj
	143   10001111 llllkkkk jjjjjjjj iiiiiiii	Push Closure Num Copied llll Num Args kkkk BlockSize jjjjjjjjiiiiiiii
This is an exact duplicate of EncoderForLongFormV3PlusClosures.
Could be a trait (or in Newspeak, a Mixin).
For now we impose upon you to synchronise any and all changes between these two classes.
"
Class {
	#name : #EncoderForV3PlusClosures,
	#superclass : #EncoderForV3,
	#category : #Kernel-BytecodeEncoders
}

{ #category : #'bytecode decoding' }
EncoderForV3PlusClosures class >> backJumpBytecodeSize [
	^ 2
]

{ #category : #'compiled method support' }
EncoderForV3PlusClosures class >> bindingReadScanBlockFor: litIndex using: scanner [
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of the value of the binding with zero-relative index litVarIndex.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	^ [:b|
		b >= 64
		and:
			[b <= 95
				ifTrue: [b - 64 = litIndex]
				ifFalse:
					[b = 128
						ifTrue: [scanner followingByte - 192 = litIndex]
						ifFalse:
							[b = 132
							 and: [(scanner followingByte between: 128 and: 159)
							 and: [scanner thirdByte = litIndex]]]]]]
]

{ #category : #'compiled method support' }
EncoderForV3PlusClosures class >> bindingWriteScanBlockFor: litVarIndex using: scanner [
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for writes of the value of the binding with zero-relative index litVarIndex.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	^ [:b|
		(b = 129 or: [b = 130])
			ifTrue: [scanner followingByte - 192 = litVarIndex]
			ifFalse:
				[b = 132
				 and: [scanner followingByte >= 224
				 and: [scanner thirdByte = litVarIndex]]]]
]

{ #category : #'instruction stream support' }
EncoderForV3PlusClosures class >> bytecodeSize: bytecode [
	"Answer the number of bytes in the bytecode."
	bytecode <= 125 ifTrue:
		[^1].
	bytecode >= 176 ifTrue:
		[^1].
	bytecode >= 160 ifTrue: "long jumps"
		[^2].
	bytecode >= 144 ifTrue: "short jumps"
		[^1].
	"extensions"
	bytecode >= 128 ifTrue:
		[^#(2 2 2 2 3 2 2 1 1 1 2 3 3 3 3 4) at: bytecode - 127].
	^nil
]

{ #category : #'bytecode decoding' }
EncoderForV3PlusClosures class >> callPrimitiveCode [
	"139	11101111	iiiiiiii jjjjjjjj	Call Primitive #iiiiiiii + (jjjjjjjj * 256)"
	^139
]

{ #category : #'compiled method support' }
EncoderForV3PlusClosures class >> firstSpecialSelectorByte [
	^ 16rAF
]

{ #category : #'compiled method support' }
EncoderForV3PlusClosures class >> instVarReadScanBlockFor: varIndexCode using: scanner [
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of the inst var with zero-relative index varIndexCode.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	^ [:b|
		b < 16
			ifTrue: [b = varIndexCode]
			ifFalse:
				[b = 128
					ifTrue: [scanner followingByte = varIndexCode and: [varIndexCode <= 63]]
					ifFalse:
						[b = 132
						 and: [(scanner followingByte between: 64 and: 95)
						 and: [scanner thirdByte = varIndexCode]]]]]
]

{ #category : #'compiled method support' }
EncoderForV3PlusClosures class >> instVarWriteScanBlockFor: varIndexCode using: scanner [
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for writes of the inst var with zero-relative index varIndexCode.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	^ [:b|
		b >= 96
		and: [b <= 103
				ifTrue: [b - 96 = varIndexCode]
				ifFalse:
					[(b = 129 or: [b = 130])
						ifTrue: [scanner followingByte = varIndexCode and: [varIndexCode <= 63]]
						ifFalse:
							[b = 132
							 and: [(scanner followingByte between: 160 and: 223)
							 and: [scanner thirdByte = varIndexCode]]]]]]
]

{ #category : #'instruction stream support' }
EncoderForV3PlusClosures class >> interpretJumpIfCondIn: anInstructionStream [
	"Double-dispatch through the encoder to select the correct conditional jump decoder for the instruction set."
	^anInstructionStream interpretV3PlusClosureJumpIfCond
]

{ #category : #'instruction stream support' }
EncoderForV3PlusClosures class >> interpretJumpIn: anInstructionStream [
	"Double-dispatch through the encoder to select the correct unconditional jump decoder for the instruction set."
	^ anInstructionStream interpretV3PlusClosureJump
]

{ #category : #'instruction stream support' }
EncoderForV3PlusClosures class >> interpretNextInstructionFor: aClient in: anInstructionStream [
	"Double-dispatch through the encoder to select the correct instruction set decoder."
	^ anInstructionStream interpretNextV3PlusClosureInstructionFor: aClient
]

{ #category : #'instruction stream support' }
EncoderForV3PlusClosures class >> isBlockReturnAt: pc in: method [
	^ (method at: pc) =  125
]

{ #category : #'instruction stream support' }
EncoderForV3PlusClosures class >> isBranchIfFalseAt: pc in: method [
	"Answer whether the bytecode at pc is a conditional branch-if-false."
	
	
	| byte |
	byte := method at: pc.
	^(byte between: 152 and: 159) or: [byte between: 172 and: 175]
]

{ #category : #'instruction stream support' }
EncoderForV3PlusClosures class >> isBranchIfTrueAt: pc in: method [
	"Answer whether the bytecode at pc is a conditional branch-if-true."
	
	| byte |
	byte := method at: pc.
	^ byte between: 168 and: 171
]

{ #category : #'instruction stream support' }
EncoderForV3PlusClosures class >> isCreateBlockAt: pc in: method [
	^ (method at: pc) = 143
]

{ #category : #'instruction stream support' }
EncoderForV3PlusClosures class >> isExtension: byte [
	"no extension byteocde in this set"
	^ false
]

{ #category : #'instruction stream support' }
EncoderForV3PlusClosures class >> isJumpAt: pc in: method [
	"Answer whether the bytecode at pc is an (unconditional) jump."

	| byte |
	byte := method at: pc.
	^ (byte between: 144 and: 151) or: [byte between: 160 and: 167]
]

{ #category : #'instruction stream support' }
EncoderForV3PlusClosures class >> isJustPopAt: pc in: method [
	"Answer whether the bytecode at pc is a pop."

	^ (method at: pc) = 135
]

{ #category : #'instruction stream support' }
EncoderForV3PlusClosures class >> isPushTempAt: pc in: method [
	| byte |
	((byte := method at: pc) between: 16 and: 31) ifTrue: [ ^ true ].
	^ byte = 128 and: [ (method at: pc + 1) >> 6 = 1 ]
]

{ #category : #'instruction stream support' }
EncoderForV3PlusClosures class >> isReturnAt: pc in: method [
	"Answer whether the bytecode at pc is a return from block."
	
	^(method at: pc) between: 120 and: 125
]

{ #category : #'instruction stream support' }
EncoderForV3PlusClosures class >> isSendAt: pc in: method [
	"Answer whether the bytecode at pc is a message-send."
	
	| byte |
	byte := method at: pc.
	^byte >= 131
	  and: [ "special send or short send" 
		byte >= 176 or: [ "long sends" byte <= 134]]
]

{ #category : #'instruction stream support' }
EncoderForV3PlusClosures class >> isStoreAt: pc in: method [
	"Answer whether the bytecode at pc is a store or store-pop."

	| byte |
	byte := method at: pc.
	^(byte between: 96 and: 142)
		and: [
			"96 103		storeAndPopReceiverVariableBytecode"
			"104 111	storeAndPopTemporaryVariableBytecode"
			byte <= 111 
			or: [
				"129		extendedStoreBytecode"
				byte >= 129
				and: [
					"130		extendedStoreAndPopBytecode"
					byte <= 130
					or: [(
							"132		doubleExtendedDoAnythingBytecode"
							byte = 132	 
							and: [ (method at: pc+1) >= 160 ])
					or: [
						"141		storeRemoteTempLongBytecode"
						byte = 141
					or: [
						"142		storeAndPopRemoteTempLongBytecode"
						byte = 142]]]]]]
]

{ #category : #'instruction stream support' }
EncoderForV3PlusClosures class >> isStorePopAt: pc in: method [
	"Answer whether the bytecode at pc is a store or store-pop."
	
	| byte |
	byte := method at: pc.
	"130		extendedStoreAndPopBytecode"
	^ byte = 130
		or: [
			"142		storeAndPopRemoteTempLongBytecode"
			byte = 142
		or: [
			"96 103		storeAndPopReceiverVariableBytecode"
			"104 111	storeAndPopTemporaryVariableBytecode"
			byte between: 96 and: 111 ]]
]

{ #category : #'compiled method support' }
EncoderForV3PlusClosures class >> markerOrNilFor: compiledMethod [
	"What is a marker method?  It is method with body like 
		'self subclassResponsibility' or '^ self subclassResponsibility' 
	used to indicate ('mark') a special property.

	Marker methods compile to bytecode like:

		9 <70> self
		10 <D0> send: <literal 1>
		11 <87> pop
		12 <78> returnSelf

	for the first form, or 

		9 <70> self
		10 <D0> send: <literal 1>
		11 <7C> returnTop

	for the second form."

	| e |
	e := compiledMethod endPC.
	(#(19 20 35) includes: e) ifFalse: [ ^ nil ].
	
	"Should have exactly 3 literals"
	(compiledMethod numLiterals = 3) ifFalse:[ ^ nil ].	
	
	"If we are on 32 bits"
	(#(19 20) includes: e) ifTrue: [ 
		(compiledMethod at: 17) =  16r70 ifFalse: [ ^ nil ].		"push self"
		(compiledMethod at: 18) = 16rD0 ifFalse: [ ^ nil ].		"send <literal 1>"
	].
	
		"If we are on 32 bits"
	(e = 35) ifTrue: [ 
		(compiledMethod at: 33) =  16r70 ifFalse: [ ^ nil].		"push self"
		(compiledMethod at: 34) = 16rD0 ifFalse: [ ^ nil ].		"send <literal 1>"
	].
	
	"If we reach this point, we have a marker method that sends self <literal 1>"
	^ compiledMethod literalAt: 1
]

{ #category : #'block closure support' }
EncoderForV3PlusClosures class >> methodReturnBytecodes [
	^ 120 to: 124
]

{ #category : #'instruction stream support' }
EncoderForV3PlusClosures class >> nonExtensionBytecodeAt: pc in: method [
	"Answer the actual bytecode at pc in method, skipping past any preceeding extensions."
	^ method at: pc
]

{ #category : #'block closure support' }
EncoderForV3PlusClosures class >> numLocalTempsForBlockAt: startpc in: method [
	"4 = size of push closure copy bytecode"
	^ BlockLocalTempCounter
		tempCountForBlockAt: startpc - 4 
		in: method 
]

{ #category : #'compiled method support' }
EncoderForV3PlusClosures class >> prepareMethod: compiledMethod forSimulationWith: numArgs [
	| xpc |
	xpc := compiledMethod initialPC.
	"long store temp"
	(compiledMethod at: xpc) = 129 
		ifTrue: [
			compiledMethod at: xpc + 1 put: (16r40 + numArgs).
			compiledMethod at: xpc + 3 put: (16r10 + numArgs)]
]

{ #category : #'bytecode decoding' }
EncoderForV3PlusClosures class >> pushNilByte [
	^ 115
]

{ #category : #'instruction stream support' }
EncoderForV3PlusClosures class >> selectorToSendOrItselfFor: anInstructionStream in: method at: pc [
	"If anInstructionStream is at a send bytecode then answer the send's selector,
	 otherwise answer anInstructionStream itself.  The rationale for answering
	 anInstructionStream instead of, say, nil, is that potentially any existing object
	 can be used as a selector, but since anInstructionStream postdates the method,
	 it can't be one of them."

	| byte byte2 |
	byte := method at: pc.
	byte < 131 ifTrue: [^ anInstructionStream].
	byte >= 176
		ifTrue: [
			"special byte or short send"
			byte >= 208
				ifTrue: [^ method literalAt: (byte bitAnd: 15) + 1]
				ifFalse: [^ Smalltalk specialSelectorAt: byte - 176 + 1]]
		ifFalse: [
			byte <= 134 
				ifTrue: [ 
					byte2 := method at: pc + 1.
					byte = 131 ifTrue: [^ method literalAt: byte2 \\ 32 + 1].
					byte = 132 ifTrue: [
						byte2 < 64 ifTrue: [^ method literalAt: ( method at: pc + 2) + 1]]	.
					byte = 133 ifTrue: [^ method literalAt: byte2 \\ 32 + 1].
					byte = 134 ifTrue: [^ method literalAt: byte2 \\ 64 + 1]]].
	^ anInstructionStream
]

{ #category : #'compiled method support' }
EncoderForV3PlusClosures class >> sendsToSuperFor: compiledMethod [
	"Answer whether the receiver sends any message to super."
	| scanner |
	scanner := InstructionStream on: compiledMethod.
	^ scanner scanFor: 
		[:instr |  instr = 16r85 or: [instr = 16r84
						and: [scanner followingByte between: 16r20 and: 16r3F]]]
]

{ #category : #'bytecode decoding' }
EncoderForV3PlusClosures class >> specialLiterals [
	^ #(true false nil -1 0 1 2) 
]

{ #category : #'compiled method support' }
EncoderForV3PlusClosures class >> supportsClosures [
	"Answer if the instruction set supports closures (contains
	 closure creation and indirect temp access bytecodes)."

	^true
]

{ #category : #'bytecode decoding' }
EncoderForV3PlusClosures class >> unusedBytecode [
	"Answer the opcode of a single-byte unused bytecode, if it exists in the encoder's bytecode set, or nil if not."
	^ 126
]

{ #category : #'as yet unclassified' }
EncoderForV3PlusClosures >> genCallPrimitive: primitiveIndex [
	"139	11101111	iiiiiiii jjjjjjjj	Call Primitive #iiiiiiii + (jjjjjjjj * 256)"
	(primitiveIndex < 1 or: [primitiveIndex > 65535]) ifTrue:
		[self outOfRangeError: 'primitive index' index: primitiveIndex range: 1 to: 65535].
	stream
		nextPut: 139;
		nextPut: (primitiveIndex bitAnd: 255);
		nextPut: (primitiveIndex bitShift: -8)
]

{ #category : #'bytecode generation' }
EncoderForV3PlusClosures >> genPushClosureCopyNumCopiedValues: numCopied numArgs: numArgs jumpSize: jumpSize [
	"143 	10001111 llllkkkk jjjjjjjj iiiiiiii	Push Closure Num Copied llll Num Args kkkk BlockSize jjjjjjjjiiiiiiii"
	(jumpSize < 0 or: [jumpSize > 65535]) ifTrue:
		[^self outOfRangeError: 'block size' index: jumpSize range: 0 to: 65535].
	(numCopied < 0 or: [numCopied > 15]) ifTrue:
		[^self outOfRangeError: 'num copied' index: numCopied range: 0 to: 15].
	(numArgs < 0 or: [numArgs > 15]) ifTrue:
		[^self outOfRangeError: 'num args' index: numArgs range: 0 to: 15].
	stream
		nextPut: 143;
		nextPut: numArgs + (numCopied bitShift: 4);
		nextPut: (jumpSize bitShift: -8);
		nextPut: (jumpSize bitAnd: 16rFF)
]

{ #category : #'bytecode generation' }
EncoderForV3PlusClosures >> genPushConsArray: size [
	(size < 0 or: [size > 127]) ifTrue:
		[^self outOfRangeError: 'numElements' index: size range: 0 to: 127].
	"138 	10001010 1kkkkkkk 	Push (Array new: kkkkkkk)"
	stream
		nextPut: 138;
		nextPut: size + 128
]

{ #category : #'bytecode generation' }
EncoderForV3PlusClosures >> genPushNewArray: size [
	(size < 0 or: [size > 127]) ifTrue:
		[^self outOfRangeError: 'numElements' index: size range: 0 to: 127].
	"138 	10001010 0kkkkkkk 	Pop kkkkkkk into: (Array new: kkkkkkk)"
	stream
		nextPut: 138;
		nextPut: size
]

{ #category : #'bytecode generation' }
EncoderForV3PlusClosures >> genPushRemoteTemp: tempIndex inVectorAt: tempVectorIndex [
	(tempIndex >= 0 and: [tempIndex < 256
	 and: [tempVectorIndex >= 0 and: [tempVectorIndex < 256]]]) ifTrue:
		["140 	10001100 kkkkkkkk jjjjjjjj 	Push Temp At kkkkkkkk In Temp Vector At: jjjjjjjj"
		 stream
			nextPut: 140;
			nextPut: tempIndex;
			nextPut: tempVectorIndex.
		 ^self].
	tempIndex >= 256 ifTrue:
		[^self outOfRangeError: 'remoteTempIndex' index: tempIndex range: 0 to: 255].
	tempVectorIndex >= 256 ifTrue:
		[^self outOfRangeError: 'tempVectorIndex' index: tempVectorIndex range: 0 to: 255]
]

{ #category : #'bytecode generation' }
EncoderForV3PlusClosures >> genStorePopRemoteTemp: tempIndex inVectorAt: tempVectorIndex [
	"142 	10001110 kkkkkkkk jjjjjjjj 	Pop and Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj"
	(tempIndex >= 0 and: [tempIndex < 256
	 and: [tempVectorIndex >= 0 and: [tempVectorIndex < 256]]]) ifTrue:
		[stream
			nextPut: 142;
			nextPut: tempIndex;
			nextPut: tempVectorIndex.
		 ^self].
	tempIndex >= 256 ifTrue:
		[^self outOfRangeError: 'remoteTempIndex' index: tempIndex range: 0 to: 255].
	tempVectorIndex >= 256 ifTrue:
		[^self outOfRangeError: 'tempVectorIndex' index: tempVectorIndex range: 0 to: 255]
]

{ #category : #'bytecode generation' }
EncoderForV3PlusClosures >> genStoreRemoteTemp: tempIndex inVectorAt: tempVectorIndex [
	"141 	10001101 kkkkkkkk jjjjjjjj 	Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj"
	(tempIndex >= 0 and: [tempIndex < 256
	 and: [tempVectorIndex >= 0 and: [tempVectorIndex < 256]]]) ifTrue:
		[stream
			nextPut: 141;
			nextPut: tempIndex;
			nextPut: tempVectorIndex.
		 ^self].
	tempIndex >= 256 ifTrue:
		[^self outOfRangeError: 'remoteTempIndex' index: tempIndex range: 0 to: 255].
	tempVectorIndex >= 256 ifTrue:
		[^self outOfRangeError: 'tempVectorIndex' index: tempVectorIndex range: 0 to: 255]
]

{ #category : #testing }
EncoderForV3PlusClosures >> supportsClosureOpcodes [
	^true
]

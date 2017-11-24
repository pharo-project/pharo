"
I add behaviour to Encoder to size and emit bytecodes for the Squeak V3.x VM bytecode set.  The intention is for another subclass to restrict the range of bytecodes used to long forms only, allowing the bytecode set to be redefined by avoiding using the many short forms.  The short forms may then be reassigned.
"
Class {
	#name : #LegacyEncoderForV3,
	#superclass : #LegacyBytecodeEncoder,
	#category : #'Compiler-Kernel'
}

{ #category : #'compiled method support' }
LegacyEncoderForV3 class >> supportsClosures [
	"Answer if the instruction set supports closures (contains
	 closure creation and indirect temp access bytecodes)."

	^ false
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genBranchPopFalse: distance [
	"See BlueBook page 596"
	distance < 0 ifTrue:
		[^self outOfRangeError: 'distance' index: distance range: 0 to: 1023].
	(distance > 0 and: [distance < 9]) ifTrue:
		["152-159 	10011iii 	Pop and Jump 0n False iii +1 (i.e., 1 through 8)"
		 stream nextPut: 152 + distance - 1.
		 ^self].
	distance < 1024 ifTrue:
		["172-175 	101011ii jjjjjjjj 	Pop and Jump On False ii *256+jjjjjjjj"
		 stream
			nextPut: 172 + (distance bitShift: -8);
			nextPut: distance + 1024 \\ 256.
		 ^self].
	^self outOfRangeError: 'distance' index: distance range: 0 to: 1023
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genBranchPopTrue: distance [
	"See BlueBook page 596"
	distance < 0 ifTrue:
		[^self outOfRangeError: 'distance' index: distance range: 0 to: 1023].
	distance < 1024 ifTrue:
		["168-171 	101010ii jjjjjjjj 	Pop and Jump On True ii *256+jjjjjjjj"
		 stream
			nextPut: 168 + (distance bitShift: -8);
			nextPut: distance + 1024 \\ 256.
		 ^self].
	^self outOfRangeError: 'distance' index: distance range: 0 to: 1023
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genDup [
	"See BlueBook page 596"
	"136 	10001000 	Duplicate Stack Top"
	stream nextPut: 136
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genJump: distance [
	"See BlueBook page 596"
	(distance > 0 and: [distance < 9]) ifTrue:
		["144-151 	10010iii 	Jump iii + 1 (i.e., 1 through 8)"
		 stream nextPut: 144 + distance - 1.
		 ^self].
	"160-167 	10100iii jjjjjjjj 	Jump(iii - 4) *256+jjjjjjjj"
	^self genJumpLong: distance
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genJumpLong: distance [
	"See BlueBook page 596"
	(distance >= -1024 and: [distance < 1024]) ifTrue:
		["160-167 	10100iii jjjjjjjj 	Jump(iii - 4) *256+jjjjjjjj"
		 stream
			nextPut: 160 + (distance + 1024 bitShift: -8);
			nextPut: distance + 1024 \\ 256.
		 ^self].
	^self outOfRangeError: 'distance' index: distance range: -1024 to: 1023
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genPop [
	"See BlueBook page 596"
	"135 	10000111 	Pop Stack Top"
	stream nextPut: 135
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genPushInstVar: instVarIndex [
	"See BlueBook page 596"
	instVarIndex >= 0 ifTrue:
		[instVarIndex < 16 ifTrue:
			["0-15 	0000iiii 	Push Receiver Variable #iiii"
			 stream nextPut: 0 + instVarIndex.
			 ^self].
		instVarIndex < 64 ifTrue:
			["128 	10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk"
			 stream
				nextPut: 128;
				nextPut: instVarIndex.
			 ^self]].
	self genPushInstVarLong: instVarIndex
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genPushInstVarLong: instVarIndex [
	"See BlueBook page 596"
	"See also MaybeContextInstanceVariableNode"
	(instVarIndex >= 0 and: [instVarIndex < 256]) ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 64;
			nextPut: instVarIndex.
		 ^self].
	^self outOfRangeError: 'index' index: instVarIndex range: 0 to: 255
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genPushLiteral: literalIndex [
	"See BlueBook page 596"
	literalIndex < 0 ifTrue: 
		[^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255].
	literalIndex < 32 ifTrue: 
		["32-63 	001iiiii 	Push Literal Constant #iiiii"
		 stream nextPut: 32 + literalIndex.
		 ^self].
	literalIndex < 64 ifTrue: 
		["128 	10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 128;
			nextPut: 128 + literalIndex.
		 ^self].
	literalIndex < 256 ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 96;
			nextPut: literalIndex.
		 ^self].
	^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genPushLiteralVar: literalIndex [
	"See BlueBook page 596"
	literalIndex < 0 ifTrue: 
		[^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255].
	literalIndex < 32 ifTrue: 
		["64-95 	010iiiii 	Push Literal Variable #iiiii"
		 stream nextPut: 64 + literalIndex.
		 ^self].
	literalIndex < 64 ifTrue: 
		["128 	10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 128;
			nextPut: 192 + literalIndex.
		 ^self].
	literalIndex < 256 ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 128;
			nextPut: literalIndex.
		 ^self].
	^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genPushReceiver [
	"See BlueBook page 596"
	"112-119 	01110iii 	Push (receiver, true, false, nil, -1, 0, 1, 2) [iii]"
	stream nextPut: 112
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genPushSpecialLiteral: aLiteral [
	"112-119 	01110iii 	Push (receiver, true, false, nil, -1, 0, 1, 2) [iii]"
	| index |
	index := #(true false nil -1 0 1 2) indexOf: aLiteral ifAbsent: 0.
	index = 0 ifTrue:
		[^self error: 'push special literal: ', aLiteral printString,  ' is not one of true false nil -1 0 1 2'].
	stream nextPut: index + 112
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genPushTemp: tempIndex [
	"See BlueBook page 596"
	tempIndex < 0 ifTrue:
		[^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63].
	tempIndex < 16 ifTrue: 
		["16-31 	0001iiii 	Push Temporary Location #iiii"
		 stream nextPut: 16 + tempIndex.
		 ^self].
	tempIndex < 64 ifTrue: 
		["128 	10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 128;
			nextPut: 64 + tempIndex.
		 ^self].
	^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genPushThisContext [
	"See BlueBook page 596"
	"137 	10001001 	Push Active Context"
	stream nextPut: 137
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genReturnReceiver [
	"See BlueBook page 596"
	"120-123 	011110ii 	Return (receiver, true, false, nil) [ii] From Message"
	stream nextPut: 120
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genReturnSpecialLiteral: aLiteral [
	"120-123 	011110ii 	Return (receiver, true, false, nil) [ii] From Message"
	| index |
	index := #(true false nil) indexOf: aLiteral ifAbsent: 0.
	index = 0 ifTrue:
		[^self error: 'return special literal: ', aLiteral printString,  ' is not one of true false nil'].
	stream nextPut: 120 + index
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genReturnTop [
	"See BlueBook page 596"
	"124-125 	0111110i 	Return Stack Top From (Message, Block) [i]"
	stream nextPut: 124
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genReturnTopToCaller [
	"See BlueBook page 596"
	"124-125 	0111110i 	Return Stack Top From (Message, Block) [i]"
	stream nextPut: 125
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genSend: selectorLiteralIndex numArgs: nArgs [
	"See BlueBook page 596 (with exceptions for 132 & 134)"
	nArgs < 0 ifTrue:
		[^self outOfRangeError: 'numArgs' index: nArgs range: 0 to: 31 "!!"].
	selectorLiteralIndex < 0 ifTrue:
		["Special selector sends.
			176-191 	1011iiii 	Send Arithmetic Message #iiii
			192-207 	1100iiii 	Send Special Message #iiii"
		self flag: #yuck.
		 (selectorLiteralIndex negated between: 176 and: 207) ifFalse:
			[^self outOfRangeError: 'special selector code' index: selectorLiteralIndex negated range: 176 to: 207].
		 stream nextPut: selectorLiteralIndex negated.
		 ^self].
	(selectorLiteralIndex < 16 and: [nArgs < 3]) ifTrue:
		["	208-223 	1101iiii 	Send Literal Selector #iiii With No Arguments
			224-239 	1110iiii 	Send Literal Selector #iiii With 1 Argument
			240-255 	1111iiii 	Send Literal Selector #iiii With 2 Arguments"
		 stream nextPut: 208 + (nArgs * 16) + selectorLiteralIndex.
		 ^self].
	(selectorLiteralIndex < 32 and: [nArgs < 8]) ifTrue: 
		["	131 	10000011 jjjkkkkk 	Send Literal Selector #kkkkk With jjj Arguments"
		 stream
			nextPut: 131;
			nextPut: ((nArgs bitShift: 5) + selectorLiteralIndex).
		 ^self].
	(selectorLiteralIndex < 64 and: [nArgs < 4]) ifTrue: 
	 	["In Squeak V3
			134 	10000110 jjjjjjjj kkkkkkkk 	Send Literal Selector #kkkkkkkk To Superclass With jjjjjjjj Arguments
		 is replaced by
			134 	10000110 jjkkkkkk 	Send Literal Selector #kkkkkk With jj Arguments"
		 stream
			nextPut: 134;
			nextPut: ((nArgs bitShift: 6) + selectorLiteralIndex).
		 ^self].
	(selectorLiteralIndex < 256 and: [nArgs < 32]) ifTrue: 
		["In Squeak V3
			132 	10000100 jjjjjjjj kkkkkkkk 	Send Literal Selector #kkkkkkkk With jjjjjjjj Arguments
		  is replaced by
			132 	10000100 ooojjjjj kkkkkkkk
				ooo = 0 => Send Literal Selector #kkkkkkkk With jjjjj Arguments
				ooo = 1 => Send Literal Selector #kkkkkkkk To Superclass With jjjjj Arguments"
		stream
			nextPut: 132;
			nextPut: nArgs;
			nextPut: selectorLiteralIndex.
		 ^self].
	nArgs >= 32 ifTrue:
		[^self outOfRangeError: 'numArgs' index: nArgs range: 0 to: 31].
	selectorLiteralIndex >= 256 ifTrue: 
		[^self outOfRangeError: 'selector literal index' index: selectorLiteralIndex range: 0 to: 255]
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genSendSpecial: specialSelectorIndex numArgs: nArgs [
	self assert: (specialSelectorIndex between: 1 and: Smalltalk specialSelectorSize).
	self assert: nArgs = (Smalltalk specialNargsAt: specialSelectorIndex).
	"Special selector sends.
		96-111		0110 iiii			Send Arithmetic Message #iiii #(#+ #- #< #> #'<=' #'>=' #= #'~=' #* #/ #'\\' #@ #bitShift: #'//' #bitAnd: #bitOr:)
		112-119	01110 iii			Send Special Message #iii #(#at: #at:put: #size ? ? ? #'==' class ? value value: ? ? ? ? ?)"

	stream nextPut: specialSelectorIndex + 175
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genSendSuper: selectorLiteralIndex numArgs: nArgs [
	"See BlueBook page 596 (with exceptions for 132 & 134)"
	nArgs < 0 ifTrue:
		[^self outOfRangeError: 'numArgs' index: nArgs range: 0 to: 31 "!!"].
	selectorLiteralIndex < 0 ifTrue:
		[^self outOfRangeError: 'selector literal index' index: selectorLiteralIndex range: 0 to: 255].
	(selectorLiteralIndex < 32 and: [nArgs < 8]) ifTrue: 
		["	133 	10000011 jjjkkkkk 	Send Literal Selector #kkkkk To Superclass With jjj Arguments"
		 stream
			nextPut: 133;
			nextPut: ((nArgs bitShift: 5) + selectorLiteralIndex).
		 ^self].
	(selectorLiteralIndex < 256 and: [nArgs < 32]) ifTrue: 
		["In Squeak V3
			132 	10000100 jjjjjjjj kkkkkkkk 	Send Literal Selector #kkkkkkkk With jjjjjjjj Arguments
		  is replaced by
			132 	10000100 ooojjjjj kkkkkkkk
				ooo = 0 => Send Literal Selector #kkkkkkkk With jjjjj Arguments
				ooo = 1 => Send Literal Selector #kkkkkkkk To Superclass With jjjjj Arguments"
		stream
			nextPut: 132;
			nextPut: 32 + nArgs;
			nextPut: selectorLiteralIndex.
		 ^self].
	nArgs >= 32 ifTrue:
		[^self outOfRangeError: 'numArgs' index: nArgs range: 0 to: 31].
	selectorLiteralIndex >= 256 ifTrue: 
		[^self outOfRangeError: 'selector literal index' index: selectorLiteralIndex range: 0 to: 255]
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genStoreInstVar: instVarIndex [
	"See BlueBook page 596"
	(instVarIndex >= 0 and: [instVarIndex < 64]) ifTrue: 
		["129 	10000001 jjkkkkkk 	Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 129;
			nextPut: instVarIndex.
		 ^self].
	self genStoreInstVarLong: instVarIndex
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genStoreInstVarLong: instVarIndex [
	"See BlueBook page 596"
	"See also MaybeContextInstanceVariableNode"
	(instVarIndex >= 0 and: [instVarIndex < 256]) ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 160;
			nextPut: instVarIndex.
		 ^self].
	^self outOfRangeError: 'index' index: instVarIndex range: 0 to: 255
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genStoreLiteralVar: literalIndex [
	"See BlueBook page 596"
	literalIndex < 0 ifTrue:
		[^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255].
	literalIndex < 64 ifTrue: 
		["129 	10000001 jjkkkkkk 	Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 129;
			nextPut: 192 + literalIndex.
		 ^self].
	literalIndex < 256 ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 224;
			nextPut: literalIndex.
		 ^self].
	^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genStorePopInstVar: instVarIndex [
	"See BlueBook page 596"
	instVarIndex >= 0 ifTrue:
		[instVarIndex < 8 ifTrue:
			["96-103 	01100iii 	Pop and Store Receiver Variable #iii"
			 stream nextPut: 96 + instVarIndex.
			 ^self].
		instVarIndex < 64 ifTrue:
			["130 	10000010 jjkkkkkk 	Pop and Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
			 stream
				nextPut: 130;
				nextPut: instVarIndex.
			 ^self]].
	self genStorePopInstVarLong: instVarIndex
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genStorePopInstVarLong: instVarIndex [
	"See BlueBook page 596"
	"See also MaybeContextInstanceVariableNode"
	(instVarIndex >= 0 and: [instVarIndex < 256]) ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 192;
			nextPut: instVarIndex.
		 ^self].
	^self outOfRangeError: 'index' index: instVarIndex range: 0 to: 255
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genStorePopLiteralVar: literalIndex [
	"See BlueBook page 596"
	literalIndex < 0 ifTrue:
		[^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255].
	literalIndex < 64 ifTrue: 
		["130 	10000010 jjkkkkkk 	Pop and Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 130;
			nextPut: 192 + literalIndex.
		 ^self].
	literalIndex < 256 ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 224;
			nextPut: literalIndex.
		 self genPop.
		 ^self].
	^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genStorePopTemp: tempIndex [
	"See BlueBook page 596"
	tempIndex < 0 ifTrue:
		[^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63].
	tempIndex < 8 ifTrue: 
		["104-111 	01101iii 	Pop and Store Temporary Location #iii"
		 stream nextPut: 104 + tempIndex.
		 ^self].
	tempIndex < 64 ifTrue: 
		["130 	10000010 jjkkkkkk 	Pop and Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 130;
			nextPut: 64 + tempIndex.
		 ^self].
	^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63
]

{ #category : #'bytecode generation' }
LegacyEncoderForV3 >> genStoreTemp: tempIndex [
	"See BlueBook page 596"
	tempIndex < 0 ifTrue:
		[^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63].
	tempIndex < 64 ifTrue: 
		["129 	10000001 jjkkkkkk 	Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 129;
			nextPut: 64 + tempIndex.
		 ^self].
	^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63
]

{ #category : #testing }
LegacyEncoderForV3 >> isSpecialLiteralForPush: literal [
	^literal == false
	  or: [literal == true
	  or: [literal == nil
	  or: [literal isInteger and: [literal between: -1 and: 2]]]]
]

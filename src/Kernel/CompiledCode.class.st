"
My instances (and subinstances) are objects suitable for execution by the virtual machine. My subclasses and I have a specific layout so the instances intermix both indexable pointer fields and indexable integer fields.

	
The current format of a CompiledCode is as follows:

	header (4 bytes)
	literals (4 bytes each)
	bytecodes  (variable)
	trailer (variable)

The header describes the compiled code. It's a small integer with the following format: 

sign bit 	1 bit: 	if set, the method is encoded in the SecondaryBytecodeSet, else in the PrimaryBytecodeSet (See class variables) 
(index 0)	15 bits:	number of literals
(index 16)	1 bit:	requires counters (in the sista JIT, methods marked this way cannot trip)
(index 17)	1 bit:	whether a large frame size is needed
(index 18)	6 bits:	number of temporary variables
(index 24)	4 bits:	number of arguments to the method
(index 28)	1 bit:	has primitive
(index 29)	1 bit:	flag bit, ignored by the VM

The trailer encodes how to fetch the method's sources. See CompiledMethodTrailer.
"
Class {
	#name : #CompiledCode,
	#superclass : #ByteArray,
	#type : #compiledMethod,
	#classVars : [
		'LargeFrame',
		'PrimaryBytecodeSetEncoderClass',
		'SecondaryBytecodeSetEncoderClass',
		'SmallFrame'
	],
	#category : #'Kernel-Methods'
}

{ #category : #'instance creation' }
CompiledCode class >> newMethod: numberOfBytes header: headerWord [
	"Primitive. Answer an instance of me. The number of literals (and other 
	 information) is specified by the headerWord (see my class comment).
	 The first argument specifies the number of fields for bytecodes in the
	 method. Fail if either argument is not a SmallInteger, or if numberOfBytes
	 is negative, or if memory is low. Once the header of a method is set by
	 this primitive, it cannot be changed to change the number of literals.
	 Essential. See Object documentation whatIsAPrimitive."

	<primitive: 79 error: ec>
	ec == #'insufficient object memory' ifTrue:
		[^self handleFailingNewMethod: numberOfBytes header: headerWord].
	^self primitiveFailed
]

{ #category : #comparing }
CompiledCode >> = aCompiledMethod [
	"Answer whether the receiver implements the same code as aCompiledMethod."

	self == aCompiledMethod
		ifTrue: [ ^ true ].
	self class = aCompiledMethod class
		ifFalse: [ ^ false ].
	self size = aCompiledMethod size
		ifFalse: [ ^ false ].
	self header = aCompiledMethod header
		ifFalse: [ ^ false ].
	self initialPC to: self endPC do: [ :i | 
		(self at: i) = (aCompiledMethod at: i)
			ifFalse: [ ^ false ] ].
	(self sameLiteralsAs: aCompiledMethod)
		ifFalse: [ ^ false ].
	^ true
]

{ #category : #scanning }
CompiledCode >> abstractBytecodeMessageAt: pc [
	"Answer the abstract bytecode message at pc in the receiver."
	^[(InstructionStream new method: self pc: pc) interpretNextInstructionFor: nil]
		on: MessageNotUnderstood
		do: [:ex| ex message]
]

{ #category : #scanning }
CompiledCode >> abstractBytecodeMessagesDo: aBlock [
	"Evaluate aBlock with the sequence of abstract bytecodes in the receiver"
	self abstractBytecodeMessagesFrom: self initialPC
		to: self endPC
		do: aBlock

	"| msgs |
	 msgs := OrderedCollection new.
	 CompiledMethod >> #abstractBytecodeMessagesFrom:to: abstractBytecodeMessagesDo:
		[:msg| msgs add: msg selector].
	 msgs"
]

{ #category : #scanning }
CompiledCode >> abstractBytecodeMessagesFrom: startpc to: endpc do: aBlock [
	"Evaluate aBlock with the sequence of abstract bytecodes from startpc through endpc in the receiver"
	| scanner |
	scanner := InstructionStream new method: self pc: startpc.
	[scanner pc <= endpc] whileTrue:
		[[scanner interpretNextInstructionFor: nil]
			on: MessageNotUnderstood
			do: [:ex| aBlock value: ex message]]

	"| m msgs |
	 msgs := OrderedCollection new.
	 (m := CompiledMethod >> #abstractBytecodeMessagesFrom:to:)
		abstractBytecodeMessagesFrom: m initialPC
		to: m endPC
		do: [:msg| msgs add: msg selector].
	 msgs"
]

{ #category : #scanning }
CompiledCode >> accessesSlot: aSlot [
	self ast nodesDo: [ :node | 
		node isVariable and: [
			node isInstance and: [ 
				(node binding slot ==  aSlot)
					 ifTrue: [^true]]]]. 
	^false
	
]

{ #category : #literals }
CompiledCode >> allLiterals [
	^self literals
]

{ #category : #'source code management' }
CompiledCode >> argumentNames [
	^self 
		propertyValueAt: #argumentNames 
		ifAbsent: [ self ast argumentNames ]

]

{ #category : #converting }
CompiledCode >> asOrderedCollection [
	"We should override it because most of collection methods are not working for CompiledMethod. And it can't be normally converted into OrderedCollection.	
	It is special class which needs to be ByteArray by VM. But it is not behaves like ByteArray from user perspective"
	
	^OrderedCollection with: self
]

{ #category : #printing }
CompiledCode >> asString [

	  ^self sourceCode
]

{ #category : #'source code management' }
CompiledCode >> author [
	"Answer the author of the current version of the receiver. retrieved from the sources or changes file. Answer the empty string if no time stamp is available."

	"(CompiledMethod compiledMethodAt: #author) author"

	self timeStamp ifNotEmpty: [:s | 
		|subS|
		subS := s substrings first. 
		subS first isLetter ifTrue:[^subS]].
	^''
]

{ #category : #accessing }
CompiledCode >> bytecode [
	"Answer an ByteArray of the btyecode of the method."

	| start stop bytecode |

	start := self initialPC.
	stop := self endPC.
	
	bytecode := ByteArray new: (stop - start + 1).
	start to: stop do: [:index |
		bytecode byteAt: index - start + 1 put: (self byteAt: index)].
	^bytecode
]

{ #category : #accessing }
CompiledCode >> clearFlag [
	"Clear the user-level flag bit"

	self objectAt: 1 put: (self header bitAnd: (1 << 29) bitInvert)
]

{ #category : #accessing }
CompiledCode >> clearSignFlag [
	"Clear the sign flag bit.  The sign flag bit may be
	 used by the VM to select an alternate bytecode set."

	self signFlag ifTrue:
		[self objectAt: 1 put: self header - SmallInteger minVal]
]

{ #category : #accessing }
CompiledCode >> comment [
	"Return the first comment of the receiver"
	"(self>>#comment) comment"
	
	^ self methodClass firstPrecodeCommentFor: self selector
]

{ #category : #testing }
CompiledCode >> containsHalt [

	^ self sendsAnySelectorOf: #( halt halt: haltIf: haltIfNil haltOnCount: haltOnce)
]

{ #category : #'source code management' }
CompiledCode >> definition [
	"Polymorphic to class definition"
	
	^ self sourceCode
]

{ #category : #accessing }
CompiledCode >> encoderClass [
	^ self signFlag 
		ifTrue: [ SecondaryBytecodeSetEncoderClass ]
		ifFalse: [ PrimaryBytecodeSetEncoderClass ]
]

{ #category : #accessing }
CompiledCode >> endPC [
	"Answer the index of the last bytecode."
	^ self trailer endPC

]

{ #category : #comparing }
CompiledCode >> equivalentTo: aCompiledMethod [
	^self = aCompiledMethod
	or: [self class == aCompiledMethod class
		and: [self numArgs = aCompiledMethod numArgs
		and: [self numLiterals = aCompiledMethod numLiterals
		and: [self methodNode = aCompiledMethod methodNode ]]]]
]

{ #category : #accessing }
CompiledCode >> flag [
	"Answer the user-level flag bit"

	^((self header bitShift: -29) bitAnd: 1) = 1
]

{ #category : #accessing }
CompiledCode >> frameSize [
	"Answer the size of temporary frame needed to run the receiver."
	"NOTE:  Versions 2.7 and later use two sizes of contexts."

	(self header noMask: 16r20000)
		ifTrue: [^ SmallFrame]
		ifFalse: [^ LargeFrame]

]

{ #category : #scanning }
CompiledCode >> hasInstVarRef [
	self localHasInstVarRef ifTrue: [ ^ true ].
	^ self innerCompiledBlocksAnySatisfy: [ :cb | cb hasInstVarRef ]
]

{ #category : #literals }
CompiledCode >> hasLiteral: literal [
	"Answer whether the receiver references the argument, literal."
	2 to: self numLiterals - 1 do: "exclude superclass + selector/properties"
		[:index |
		| lit |
		((lit := self objectAt: index) literalEqual: literal) ifTrue: [^true].
		(lit isMemberOf: CompiledBlock) ifTrue: [ (lit hasLiteral: literal) ifTrue: [ ^true ] ]].
	^false
]

{ #category : #literals }
CompiledCode >> hasLiteralSuchThat: litBlock [
	"Answer true if litBlock returns true for any literal in this method, even if embedded in array structure."
	(self penultimateLiteral isMethodProperties
	 and: [self penultimateLiteral hasLiteralSuchThat: litBlock]) ifTrue:
		[^true].
	2 to: self numLiterals + 1 do:
		[:index | | lit |
		lit := self objectAt: index.
		((litBlock value: lit)
		or: [lit isArray and: [lit hasLiteralSuchThat: litBlock]]) ifTrue:
			[^true].
		(lit isMemberOf: CompiledBlock) ifTrue: [ (lit hasLiteralSuchThat: litBlock) ifTrue: [ ^true ] ]].
	^false
]

{ #category : #literals }
CompiledCode >> hasLiteralThorough: literal [
	"Answer true if any literal in this method is literal,
	even if embedded in array structure."

	(self penultimateLiteral isMethodProperties
	 and: [self penultimateLiteral hasLiteralThorough: literal]) ifTrue:[^true].
	2 to: self numLiterals - 1 "exclude header and methodClass or outerCode"
	   do:[:index | | lit |
		(((lit := self objectAt: index) literalEqual: literal)
		 or: [lit isArray and: [lit hasLiteral: literal]]) ifTrue:
			[^ true].
		(lit isMemberOf: CompiledBlock) ifTrue: [ (lit hasLiteralThorough: literal) ifTrue: [ ^true ] ] ].
	^ false 
]

{ #category : #literals }
CompiledCode >> hasSelector: selector [ 
	"Answers true if the method refers to the selector"
	^ self hasSelector: selector specialSelectorIndex: (Smalltalk specialSelectorIndexOrNil: selector)
]

{ #category : #literals }
CompiledCode >> hasSelector: selector specialSelectorIndex: specialOrNil [
	"Answers true if the method refers to the selector.
	 If you don't know what's a special selector, use #hasSelector:.
	 If you do, you may call this method directly to avoid recomputing 
	 the special selector index all the time."
	 (self hasLiteralThorough: selector) ifTrue: [ ^ true ].
	^ specialOrNil 
		ifNil: [ false ] 
		ifNotNil: [ self scanFor: self encoderClass firstSpecialSelectorByte + specialOrNil ]
]

{ #category : #comparing }
CompiledCode >> hash [
	"CompiledMethod>>#= compares code, i.e. same literals and same bytecode.
	 So we look at the header, methodClass and some bytes between initialPC and endPC,
	 but /not/ the selector because the equal method does not compare selectors.
	 Note that we must override ByteArray>hash which looks at all bytes of the receiver.
	 Using bytes from the pointer part of a COmpiledmethod can lead to a variable hash
	 if and when when the GC moves literals in the receiver."
	| initialPC endPC hash |
	initialPC := self initialPC.
	endPC := self endPC.
	hash := self species hash + self header + initialPC + endPC + self methodClass hash bitAnd: 16rFFFFFFF.
	"sample approximately 20 bytes"
	initialPC to: endPC by: (endPC - initialPC // 20 max: 1) do:
		[:i| hash := hash + (self at: i)].
	^hash

	"(CompiledMethod>>#hash) hash"

]

{ #category : #literals }
CompiledCode >> header [
	"Answer the word containing the information about the form of the 
	receiver and the form of the context needed to run the receiver."

	^self objectAt: 1
]

{ #category : #literals }
CompiledCode >> headerDescription [
	"Answer a description containing the information about the form of the 
	receiver and the form of the context needed to run the receiver."

	| s |
	s := '' writeStream.
	self header printOn: s.
	s cr; nextPutAll: '"signFlag: '.
	self signFlag printOn: s.
	s nextPutAll: ' ('.
	self encoderClass name printOn: s.
	s nextPut: $).
	s cr; nextPutAll: 'primitive: '.
	self primitive printOn: s.
	s cr; nextPutAll: ' numArgs: '.
	self numArgs printOn: s.
	s cr; nextPutAll: ' numTemps: '.
	self numTemps printOn: s.
	s cr; nextPutAll: ' numLiterals: '.
	self numLiterals printOn: s.
	s cr; nextPutAll: ' frameSize: '.
	self frameSize printOn: s.
	s nextPut: $"; cr.
	^ s contents
]

{ #category : #literals }
CompiledCode >> indexOfLiteral: literal [
	"Answer the literal index of the argument, literal, or zero if none."
	2 to: self numLiterals - 1 "exclude superclass + selector/properties"
	   do:
		[:index |
		literal == (self objectAt: index) ifTrue: [^index - 1]].
	^0
]

{ #category : #accessing }
CompiledCode >> initialPC [
	"Answer the program counter for the receiver's first bytecode."

	^ (self numLiterals + 1) * Smalltalk wordSize + 1
]

{ #category : #literals }
CompiledCode >> innerCompiledBlocksAnySatisfy: aBlock [
	self innerCompiledBlocksDo: [ :cb | (aBlock value: cb) ifTrue: [ ^ true ] ].
	^ false
]

{ #category : #literals }
CompiledCode >> innerCompiledBlocksDo: aBlock [
	"We ignore the enclosing compiled code in compiled block by ignoring the last literal"
	1 to: self numLiterals - 1 do:
		[:index | | lit |
		lit := self objectAt: index + 1.
		(lit isMemberOf: CompiledBlock) ifTrue: [ aBlock value: lit ] ]
]

{ #category : #testing }
CompiledCode >> isCollection [
	"We should override it because most of collection methods are not working for CompiledMethod. It is special class which needs to be ByteArray by VM. But it is not behaves like ByteArray from user perspective.
	And some tools uses isCollection check for specific behaviour which will fail for CompiledMethod"
	^false
]

{ #category : #testing }
CompiledCode >> isExternalCallPrimitive [
	^self primitive = 120
]

{ #category : #testing }
CompiledCode >> isNamedPrimitive [
	^self primitive = 117
]

{ #category : #testing }
CompiledCode >> isPrimitive [
	^self primitive > 0
]

{ #category : #testing }
CompiledCode >> isQuick [
	"Answer whether the receiver is a quick return (of self or of an instance 
	variable)."
	^ self primitive between: 256 and: 519
]

{ #category : #testing }
CompiledCode >> isRealPrimitive [
	^self isPrimitive and: [ self isQuick not ]
]

{ #category : #literals }
CompiledCode >> literalAt: index [ 
	"Answer the literal indexed by the argument."

	^self objectAt: index + 1
]

{ #category : #literals }
CompiledCode >> literalAt: index put: value [ 
	"Replace the literal indexed by the first argument with the second 
	argument. Answer the second argument."

	^self objectAt: index + 1 put: value
]

{ #category : #literals }
CompiledCode >> literalStrings [
	"Return a list of strings or symbols corresponding to the class references and message sent.
	The result also contains the name of the class"
	
	| litStrs |
	litStrs := OrderedCollection new: self numLiterals.
	self literalsDo:
		[:lit | 
		(lit isVariableBinding)
			ifTrue: [litStrs addLast: lit key]
			ifFalse: [(lit isSymbol)
				ifTrue: [litStrs addAll: lit keywords]
				ifFalse: [litStrs addLast: lit printString]]].
	^ litStrs
]

{ #category : #literals }
CompiledCode >> literals [
	"Answer an Array of the literals referenced by the receiver."
	| literals numberLiterals |
	literals := Array new: (numberLiterals := self numLiterals).
	1 to: numberLiterals do: [:index |
		literals at: index put: (self objectAt: index + 1)].
	^literals
]

{ #category : #literals }
CompiledCode >> literalsDo: aBlock [
	"Evaluate aBlock for each of the literals referenced by the receiver."
	1 to: self numLiterals do:
		[:index |
		aBlock value: (self objectAt: index + 1)]
]

{ #category : #scanning }
CompiledCode >> localHasInstVarRef [
	"Answer whether the method references an instance variable."

	| scanner end printer |

	scanner := InstructionStream on: self.
	printer := InstVarRefLocator new.
	end := self endPC.

	[scanner pc <= end] whileTrue: [
		(printer interpretNextInstructionUsing: scanner) ifTrue: [^true].
	].
	^false
]

{ #category : #scanning }
CompiledCode >> localMessages [
	"Answer a Set of all the message selectors sent by this method."

	| scanner aSet |
	aSet := IdentitySet new.
	scanner := InstructionStream on: self.
	scanner	scanFor: [:x | 
			scanner addSelectorTo: aSet.
			false	"keep scanning"].
	^aSet
]

{ #category : #scanning }
CompiledCode >> localReadsRef: literalAssociation [ 
	"Answer whether the receiver loads the argument."
	| litIndex scanner |
	(litIndex := self indexOfLiteral: literalAssociation) = 0 ifTrue:
		[^false].
	litIndex := litIndex - 1.
	^(scanner := InstructionStream on: self) scanFor: (self encoderClass bindingReadScanBlockFor: litIndex using: scanner)
]

{ #category : #testing }
CompiledCode >> localSendsAnySelectorOf: aCollection [
	
	self literalsDo: [ :lit |
		(lit isSymbol and: [ aCollection includes: lit ]) 
			ifTrue: [ ^ self messages includesAny: aCollection ]
	].

	^ false 

]

{ #category : #scanning }
CompiledCode >> localSendsToSuper [
	^ self encoderClass sendsToSuperFor: self
]

{ #category : #scanning }
CompiledCode >> localWritesRef: literalAssociation [ 
	"Answer whether the receiver stores into the argument."
	| litIndex scanner |
	(litIndex := self indexOfLiteral: literalAssociation) = 0 ifTrue:
		[^false].
	litIndex := litIndex - 1.
	^(scanner := InstructionStream on: self) scanFor: (self encoderClass bindingWriteScanBlockFor: litIndex using: scanner)
]

{ #category : #printing }
CompiledCode >> longPrintOn: aStream [
	"List of all the byte codes in a method with a short description of each" 

	self isQuick ifTrue: 
		[self isReturnSpecial ifTrue:
			[^ aStream nextPutAll: 'Quick return ' , 
				(#('self' 'true' 'false' 'nil' '-1' '0' '1' '2') at: self primitive - 255)].
		^ aStream nextPutAll: 'Quick return field ' , self returnField printString , ' (0-based)'].

	self isPrimitive ifTrue: [
		self printPrimitiveOn: aStream.
	].
	self symbolicBytecodes do: [ :each | each printOn: aStream ] separatedBy: [ aStream cr ]

]

{ #category : #scanning }
CompiledCode >> messages [
	"Answer a Set of all the message selectors sent by this method."
	| aSet |
	aSet := self localMessages.
	self innerCompiledBlocksDo: [ :cb | aSet := aSet , cb messages ].
	^ aSet 
]

{ #category : #'initialize-release' }
CompiledCode >> needsFrameSize: newFrameSize [
	"Set the largeFrameBit to accomodate the newFrameSize"
	(self numTemps + newFrameSize) > LargeFrame ifTrue:
		[^ self error: 'Cannot compile -- stack including temps is too deep'].
	self setFrameBit: ((self numTemps + newFrameSize) > SmallFrame 
		or: [ self primitive = 84 "perform:withArguments:"])
]

{ #category : #accessing }
CompiledCode >> numArgs [
	"Answer the number of arguments the receiver takes."

	^ (self header bitShift: -24) bitAnd: 16r0F
]

{ #category : #accessing }
CompiledCode >> numLiterals [
	"Answer the number of literals used by the receiver."
	^self header bitAnd: 16r7FFF
]

{ #category : #accessing }
CompiledCode >> numTemps [
	"Answer the number of temporary variables used by the receiver."
	
	^ (self header bitShift: -18) bitAnd: 16r3F
]

{ #category : #literals }
CompiledCode >> objectAt: index [ 
	"Primitive. Answer the method header (if index=1) or a literal (if index 
	>1) from the receiver. Essential. See Object documentation 
	whatIsAPrimitive."

	<primitive: 68>
	self primitiveFailed
]

{ #category : #literals }
CompiledCode >> objectAt: index put: value [ 
	"Primitive. Store the value argument into a literal in the receiver. An 
	index of 2 corresponds to the first literal. Fails if the index is less than 2 
	or greater than the number of literals. Answer the value as the result. 
	Normally only the compiler sends this message, because only the 
	compiler stores values in CompiledMethods. Essential. See Object 
	documentation whatIsAPrimitive."

	<primitive: 69>
	self primitiveFailed
]

{ #category : #'debugger support' }
CompiledCode >> pcPreviousTo: pc [
	| scanner client prevPc |
	pc > self endPC ifTrue: [^self endPC].
	scanner := InstructionStream on: self.
	client := InstructionClient new.
	[scanner pc < pc] whileTrue:
		[prevPc := scanner pc.
		 scanner interpretNextInstructionFor: client].
	^prevPc
]

{ #category : #printing }
CompiledCode >> primitiveErrorVariableName [
	"Answer the primitive error code temp name, or nil if none."
	self isPrimitive ifTrue:
		[self pragmas do:
			[:pragma| | kwds ecIndex |
			((kwds := pragma keyword keywords) first = 'primitive:'
			and: [(ecIndex := kwds indexOf: 'error:') > 0]) ifTrue:
				[^pragma argumentAt: ecIndex]]].
	^nil
]

{ #category : #scanning }
CompiledCode >> readsField: varIndex [ 
	"Answer whether the receiver loads the instance variable indexed by the argument."
	| varIndexCode scanner |
	varIndexCode := varIndex - 1.
	((scanner := InstructionStream on: self) scanFor: (self encoderClass instVarReadScanBlockFor: varIndexCode using: scanner)) ifTrue: [ ^ true ].
	^ self innerCompiledBlocksAnySatisfy: [ :cb | cb readsField: varIndex ]
]

{ #category : #scanning }
CompiledCode >> readsRef: literalAssociation [ 
	"Answer whether the receiver loads the argument."
	(self localReadsRef: literalAssociation) ifTrue: [ ^ true ].
	^ self innerCompiledBlocksAnySatisfy: [ :cb | cb readsRef: literalAssociation ]
]

{ #category : #scanning }
CompiledCode >> readsSlot: aSlot [
	| nodes |
	nodes := self ast allChildren.
	nodes := nodes select: #isVariable.
	nodes := nodes select: #isInstance.
	nodes := nodes reject: [ :node | node parent isAssignment and: [ node parent variable = node ] ].
	^ nodes anySatisfy: [ :node | node binding slot == aSlot ]
]

{ #category : #accessing }
CompiledCode >> referredInstVars [
	| allInstVarNames instVarNames |
	allInstVarNames := self methodClass allInstVarNames.
	self isReturnField ifTrue:
		[^Set with: (allInstVarNames at: self returnField + 1)].
	instVarNames := Set new.
	self abstractBytecodeMessagesDo:
		[:msg|
		(#(#popIntoReceiverVariable:
		    #pushReceiverVariable:
		    #storeIntoReceiverVariable:) includes: msg selector) ifTrue:
			[instVarNames add: (allInstVarNames at: msg argument + 1)]].
	^instVarNames

	"Dictionary fromPairs: (Point selectors collect: [:s| { s. (Point >> s) referredInstVars}])".
]

{ #category : #literals }
CompiledCode >> refersToLiteral: aLiteral [ 
	"Answer true if any literal in this method is literal, even if embedded in array structure or within its pragmas."
	"only iterate to numLiterals - 1, as the last has the classBinding and the last-but-one needs special treatment"
	
	"to deprecate: use hasLiteralThorough:"
	^ self hasLiteralThorough: aLiteral.
]

{ #category : #comparing }
CompiledCode >> sameLiteralsAs: method [
	"Compare my literals to those of method. This is needed to compare compiled methods."

	| numLits literal1 literal2 |
	(numLits := self numLiterals) ~= method numLiterals
		ifTrue: [ ^ false ].
	"The last literal requires special checking instead of using #literalEqual:"
	1 to: numLits - 1 do: [ :index | 
		literal1 := self literalAt: index.
		literal2 := method literalAt: index.
		(literal1 == literal2 or: [ literal1 literalEqual: literal2 ])
			ifFalse: [ 
				(index = 1 and: [ self isNamedPrimitive | self isExternalCallPrimitive ])
					ifTrue: [ 
						literal1 isArray
							ifTrue: [ 
								(literal2 isArray and: [ literal1 allButLast = literal2 allButLast ])
									ifFalse: [ ^ false ] ]
							ifFalse: [ 
								"ExternalLibraryFunction"
								(literal1 analogousCodeTo: literal2)
									ifFalse: [ ^ false ] ] ]
					ifFalse: [ 
						index = (numLits - 1)
							ifTrue: [ 
								"properties"
								"don't create properties if they don't exist"
								(literal1 isSymbol and: [ literal2 isSymbol ]) ifFalse: [
									(self properties analogousCodeTo: method properties) ifFalse: [ ^false ] ] ]
							ifFalse: [ ^ false ] ] ] ].
	"Class side methods have non unique (nil -> a Metaclass) as literal and cannot be compared equal"
	literal1 := self literalAt: numLits.
	literal2 := method literalAt: numLits.
	^literal1 class == literal2 class
	     and: [literal1 isVariableBinding
	                ifTrue: [literal1 key = literal2 key and: [literal1 value = literal2 value]]
	                ifFalse: [literal1 = literal2]]
]

{ #category : #scanning }
CompiledCode >> scanFor: byte [ 
	"Answer whether the receiver contains the argument as a bytecode."

	^ (InstructionStream on: self) scanFor: [:instr | instr = byte]
"
Smalltalk browseAllSelect: [:m | m scanFor: 134]
"
]

{ #category : #accessing }
CompiledCode >> scanner [

	^ InstructionStream on: self
]

{ #category : #testing }
CompiledCode >> sendsAnySelectorOf: aCollection [
	
	(self localSendsAnySelectorOf: aCollection) ifTrue: [ ^ true ].
	^ self innerCompiledBlocksAnySatisfy: [ :cb | cb sendsAnySelectorOf: aCollection ]

]

{ #category : #literals }
CompiledCode >> sendsSelector: aSymbol [ 	
	"Answer whether the method sends a particular selector"
	^ self messages includes: aSymbol
]

{ #category : #scanning }
CompiledCode >> sendsToSuper [
	self localSendsToSuper ifTrue: [ ^ true ].
	^ self innerCompiledBlocksAnySatisfy: [ :cb | cb sendsToSuper ]
]

{ #category : #'initialize-release' }
CompiledCode >> setFrameBit: boolean [ 
	"true for large frame, else false"
	| largeFrameBit newHeader |
	largeFrameBit := 16r20000.
	newHeader := self header bitAnd: largeFrameBit bitInvert.
	boolean ifTrue: [ newHeader := newHeader + largeFrameBit ].
	self objectAt: 1 put: newHeader.
]

{ #category : #accessing }
CompiledCode >> setSignFlag [
	"Set the sign flag bit.  The sign flag bit may be
	 used by the VM to select an alternate bytecode set."

	self signFlag ifFalse:
		[self objectAt: 1 put: self header + SmallInteger minVal]
]

{ #category : #accessing }
CompiledCode >> signFlag [
	"Answer the sign flag bit.  The sign flag bit may be
	 used by the VM to select an alternate bytecode set."

	^ self header < 0
]

{ #category : #'source code management' }
CompiledCode >> sourcePointer [
	^ self subclassResponsibility 
]

{ #category : #'source code management' }
CompiledCode >> stamp [

	^ self timeStamp
]

{ #category : #printing }
CompiledCode >> symbolic [
	"Answer a String that contains a list of all the byte codes in a method with a short description of each."

	^self longPrintString.
]

{ #category : #printing }
CompiledCode >> symbolicBytecodes [
	"Answer Collection that contains of all the byte codes in a method as an instance of SymbolicInstruction"

	^SymbolicBytecodeBuilder decode: self
]

{ #category : #printing }
CompiledCode >> timeStamp [
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file. Answer the empty string if no time stamp is available."
	"(CompiledMethod compiledMethodAt: #timeStamp) timeStamp"

	^ SourceFiles timeStampAt: self sourcePointer.
]

{ #category : #accessing }
CompiledCode >> trailer [
	"Answer the receiver's trailer"
	^ CompiledMethodTrailer new method: self

]

{ #category : #testing }
CompiledCode >> usesPrimaryBytecodeSet [
	^ self header >= 0
]

{ #category : #testing }
CompiledCode >> usesSecondaryBytecodeSet [
	^ self header < 0
]

{ #category : #cleaning }
CompiledCode >> voidCogVMState [
	"Tell the VM to remove all references to any machine code form of the method.
	 This primitive must be called whenever a method is in use and modified.  This is
	 more aggressive (and *much* more costly) than flushCache since it must search
	 through all context objects, making sure that none have a (hidden) machine code pc
	 in the receiver.  Since modifying a method will likely change the generated machine code,
	 modifying a method (rather than redefining it) requires this more aggressive flush."

	<primitive: 215>
	^self flushCache
]

{ #category : #scanning }
CompiledCode >> writesField: varIndex [
	"Answer whether the receiver stores into the instance variable indexed by the argument."

	| varIndexCode scanner |
	varIndexCode := varIndex - 1.
	((scanner := InstructionStream on: self) scanFor: (self encoderClass instVarWriteScanBlockFor: varIndexCode using: scanner)) ifTrue: [ ^ true ].
	^ self innerCompiledBlocksAnySatisfy: [ :cb | cb writesField: varIndex ]
]

{ #category : #scanning }
CompiledCode >> writesRef: literalAssociation [ 
	"Answer whether the receiver stores into the argument."
	(self localWritesRef: literalAssociation) ifTrue: [ ^ true ].
	^ self innerCompiledBlocksAnySatisfy: [ :cb | cb writesRef: literalAssociation ]
]

{ #category : #scanning }
CompiledCode >> writesSlot: aSlot [
	self ast nodesDo: [ :node | 
		node isVariable and: [
			node parent isAssignment and: [  
				(node parent variable = node) and: [  
					node isInstance and: [
						(node binding slot ==  aSlot)
					 		ifTrue: [^true]]]]]]. 
	^false
]

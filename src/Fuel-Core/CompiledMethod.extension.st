Extension { #name : #CompiledMethod }

{ #category : #'*Fuel-Core' }
CompiledMethod >> abstractBytecodeMessagesFrom: startpc to: endpc [
	"Answer an OrderedCollection of the abstract bytecodes sent in the receiver."
	| scanner abstractBytecodes |
	scanner := InstructionStream new method: self pc: startpc.
	"now collect all the bytecode messages in the block."
	abstractBytecodes := OrderedCollection new.
	[scanner pc <= endpc] whileTrue:
		[[scanner interpretNextInstructionFor: nil]
			on: MessageNotUnderstood
			do: [:ex| abstractBytecodes add: ex message]].
	^abstractBytecodes

	"| m | (m := CompiledMethod >> #abstractBytecodeMessagesFrom:to:) abstractBytecodeMessagesFrom: m initialPC to: m endPC"
]

{ #category : #'*Fuel-Core' }
CompiledMethod >> bytecodesHash [
	"Answer a 16-bit checksum of the bytecodes."

	^ CRC crc16FromCollection: (ByteArray 
		new: self size 
		streamContents: [ :stream | self 
			from: self initialPC 
			to: self endPC 
			do: [ :byte | stream nextPut: byte ] ])
]

{ #category : #'*Fuel-Core' }
CompiledMethod >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitCompiledMethod: self
]

{ #category : #'*Fuel-Core' }
CompiledMethod >> fuelPrepare [
	"Prepare for Fuel serialization."
	
	self isNamedPrimitive ifTrue: [self literals first at: 4 put: 0].
	
	"When the method is a named primitive, the first literal is an array that works as an argument for the VM. The first and second elements represent the module and the name of the primivite. The third element of the array is ignored, it used to be a session ID. The forth element is the primitive index in a externalPrimitiveTable and it is IMPORTANT to put it in zero, otherwise there can be errors. Putting a zero means that the VM will not try to directly execute the primitive from the table and it will always try to load the module. It is the safer thing to do. For more details, read the method #primitiveExternalCall in VMMaker."
]

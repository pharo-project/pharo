Extension { #name : 'CompiledCode' }

{ #category : '*Debugging-Core' }
CompiledCode >> pcPreviousTo: pc [
	| scanner prevPc |
	pc > self endPC ifTrue: [^self endPC].
	scanner := InstructionStream on: self.
	[scanner pc < pc] whileTrue: [
		prevPc := scanner pc.
		scanner advanceToFollowingPc ].
	^prevPc
]

{ #category : '*Debugging-Core' }
CompiledCode >> rangeForPC: aPC [	
 	"return which code to hightlight in the debugger"		
 	^(self sourceNodeForPC: aPC) debugHighlightRange
]

{ #category : '*Debugging-Core' }
CompiledCode >> symbolic [
	"List of all the byte codes in a method with a short description of each"

	^ String streamContents: [ :aStream |
		self isQuick ifTrue: [
			self isReturnSpecial ifTrue: [ ^ aStream nextPutAll: 'Quick return ' , (#( 'self' 'true' 'false' 'nil' '-1' '0' '1' '2' ) at: self primitive - 255) ].
			^ aStream nextPutAll: 'Quick return field ' , self returnField printString , ' (0-based)' ].

		self isPrimitive ifTrue: [ self printPrimitiveOn: aStream ].
		self symbolicBytecodes do: [ :each | each printOn: aStream ] separatedBy: [ aStream cr ] ]
]

{ #category : '*Debugging-Core' }
CompiledCode >> symbolicBytecodes [
	"Answer Collection that contains of all the byte codes in a method as an instance of SymbolicInstruction"

	^SymbolicBytecodeBuilder decode: self
]

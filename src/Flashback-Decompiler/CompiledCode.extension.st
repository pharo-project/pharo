Extension { #name : 'CompiledCode' }

{ #category : '*Flashback-Decompiler' }
CompiledCode >> ir [
	"We as the AST for the IR... for decompiling ir from bytecode, look at IRBytecodeDecompiler"
	^ self ast ir
]

{ #category : '*Flashback-Decompiler' }
CompiledCode >> irPrimitive [

	| primNode n |
	primNode := OCIRPrimitive new num: (n := self primitive).
	(n = 117 or: [n = 120]) ifTrue: [
		primNode spec: (self literalAt: 1)].
	^ primNode
]

{ #category : '*Flashback-Decompiler' }
CompiledCode >> symbolic [
	"List of all the byte codes in a method with a short description of each"

	^ String streamContents: [ :aStream |
		self isQuick ifTrue: [
			self isReturnSpecial ifTrue: [ ^ aStream nextPutAll: 'Quick return ' , (#( 'self' 'true' 'false' 'nil' '-1' '0' '1' '2' ) at: self primitive - 255) ].
			^ aStream nextPutAll: 'Quick return field ' , self returnField printString , ' (0-based)' ].

		self isPrimitive ifTrue: [ self printPrimitiveOn: aStream ].
		self symbolicBytecodes do: [ :each | each printOn: aStream ] separatedBy: [ aStream cr ] ]
]

{ #category : '*Flashback-Decompiler' }
CompiledCode >> symbolicBytecodes [
	"Answer Collection that contains of all the byte codes in a method as an instance of SymbolicInstruction"

	^SymbolicBytecodeBuilder decode: self
]

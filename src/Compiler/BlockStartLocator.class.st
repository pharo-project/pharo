"
Locates the start of a block
"
Class {
	#name : #BlockStartLocator,
	#superclass : #InstructionClient,
	#instVars : [
		'nextJumpIsAroundBlock'
	],
	#category : 'Compiler-Support'
}

{ #category : #initialization }
BlockStartLocator >> initialize [
	super initialize.
	nextJumpIsAroundBlock := false
]

{ #category : #'instruction decoding' }
BlockStartLocator >> jump: offset [
	"If this jump is around a block answer the size of that block."

	nextJumpIsAroundBlock ifTrue:
		[nextJumpIsAroundBlock := false.
		 ^offset]
]

{ #category : #'instruction decoding' }
BlockStartLocator >> pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize [
	"Answer the size of the block"
	^blockSize
]

{ #category : #'instruction decoding' }
BlockStartLocator >> send: selector super: supered numArgs: numberArguments [
	nextJumpIsAroundBlock := #closureCopy:copiedValues: == selector
	"Don't use
		nextJumpIsAroundBlock := #(blockCopy: closureCopy:copiedValues:) includes: selector
	 since BlueBook BlockContexts do not have their own temps."
]

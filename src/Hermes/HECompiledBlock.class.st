Class {
	#name : #HECompiledBlock,
	#superclass : #HEExportedLiteral,
	#instVars : [
		'encoderClassName',
		'numArgs',
		'numTemps',
		'hasPrimitive',
		'bytecode',
		'literals'
	],
	#category : #'Hermes-Model'
}

{ #category : #accessing }
HECompiledBlock class >> tag [

	^ 28
]

{ #category : #reading }
HECompiledBlock >> asLiteralIn: anEnvironment [

	| containingBlockOrMethod newBlock literalSpace previousBlock|
	
	previousBlock := anEnvironment newBlock.
	containingBlockOrMethod := previousBlock ifNil: [ anEnvironment newMethod ].
	
	newBlock := CompiledBlock newMethod: bytecode size header: (self headerFor: anEnvironment).
	
	anEnvironment newBlock: newBlock.

	literals
		doWithIndex:
			[ :literal :idx | newBlock literalAt: idx put: (literal asLiteralIn: anEnvironment) ].

	newBlock literalAt: literals size put: containingBlockOrMethod.
	literalSpace := (literals size + 1) * Smalltalk wordSize.

 	bytecode doWithIndex: [ :e :idx | newBlock at: idx + literalSpace put: e ].

	anEnvironment newBlock: previousBlock.

	^ newBlock
]

{ #category : #reading }
HECompiledBlock >> doReadFrom: aReader [

	| numberOfLiterals |

	encoderClassName := aReader readByteString.
	numArgs := aReader readUInt32.
	numTemps := aReader readUInt32.
	hasPrimitive := aReader readUInt32.

	bytecode := aReader readByteArray.

	numberOfLiterals := aReader readInt32.
	literals := Array new: numberOfLiterals.
	1 to: numberOfLiterals do: [ :idx | literals at:idx put: (HEExportedLiteral readFrom: aReader) ]
]

{ #category : #compiling }
HECompiledBlock >> headerFor: anEnvironment [
	| encoderClass|
	"When a method is deserialized its header should be recalculated"
	encoderClass := anEnvironment classNamed: encoderClassName.
	
	^ (CompiledMethod headerFlagForEncoder: encoderClass) + (numArgs bitShift: 24) + (numTemps bitShift: 18) + literals size + (hasPrimitive bitShift: 16)
]

{ #category : #accessing }
HECompiledBlock >> value: aCompiledBlock [
	
	| literalSpace |

	encoderClassName := self encoderClassName: aCompiledBlock.
	
	numArgs := aCompiledBlock numArgs.
	numTemps := aCompiledBlock numTemps.
	hasPrimitive := aCompiledBlock isPrimitive asBit.

	literalSpace := (aCompiledBlock numLiterals + 1) * Smalltalk wordSize.

	bytecode := ByteArray new: aCompiledBlock size - literalSpace.
	1 to: bytecode size do: [ :i | bytecode at: i put: (aCompiledBlock at: i + literalSpace) ].

	literals := aCompiledBlock allLiterals allButLast collect: [ :e | e asExportedLiteral ].
	"The last literal is the home method or block of this compiledBlock"
	literals := literals copyWith: nil asExportedLiteral.
]

Class {
	#name : #HEConstantBlock,
	#superclass : #HEExportedLiteral,
	#instVars : [
		'literal',
		'numArgs',
		'compiledBlock'
	],
	#category : #'Hermes-Model'
}

{ #category : #accessing }
HEConstantBlock class >> tag [

	^ 29
]

{ #category : #reading }
HEConstantBlock >> asLiteralIn: anEnvironment [

	^ (ConstantBlockClosure
		   numArgs: numArgs
		   literal: (literal asLiteralIn: anEnvironment))
		  compiledBlock: (compiledBlock asLiteralIn: anEnvironment);
		  yourself
]

{ #category : #reading }
HEConstantBlock >> doReadFrom: aReader [

	numArgs := aReader readUInt32.
	literal := HEExportedLiteral readFrom: aReader.
	compiledBlock := HEExportedLiteral readFrom: aReader
]

{ #category : #comparing }
HEConstantBlock >> instVarNamesForEqualityComparison [

	"Ignoring the value, as it is an object of the source environment"
	^ self class allInstVarNames reject: [ :e | e = #value ]
]

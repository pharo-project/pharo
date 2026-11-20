"
I represent a clean block: a block that does not use self, the instance variables, thisContext, the outer temporaries nor a non-local return, so the compiler creates it once and stores it as a literal of the method (a CleanBlockClosure).
The compiler creates clean blocks by default since Pharo 14.

I am stored as my compiled block, and I am rebuilt from it in the target environment.
"
Class {
	#name : 'HECleanBlock',
	#superclass : 'HEExportedLiteral',
	#instVars : [
		'compiledBlock'
	],
	#category : 'Hermes-Model',
	#package : 'Hermes',
	#tag : 'Model'
}

{ #category : 'accessing' }
HECleanBlock class >> tag [

	^ 30
]

{ #category : 'reading' }
HECleanBlock >> asLiteralIn: anEnvironment [

	^ CleanBlockClosure compiledBlock: (compiledBlock asLiteralIn: anEnvironment)
]

{ #category : 'reading' }
HECleanBlock >> doReadFrom: aReader [

	compiledBlock := HEExportedLiteral readFrom: aReader
]

{ #category : 'comparing' }
HECleanBlock >> instVarNamesForEqualityComparison [
	"Ignoring the value, as it is an object of the source environment"

	^ self class allInstVarNames reject: [ :name | name = #value ]
]

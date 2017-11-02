"
The abstract  class of all the Hermes tests. It creates a reader and a writer .
"
Class {
	#name : #HEAbstractBinaryClassBuilderTest,
	#superclass : #TestCase,
	#instVars : [
		'filename',
		'reader',
		'writer',
		'fileReference'
	],
	#category : 'Hermes-Tests'
}

{ #category : #initialization }
HEAbstractBinaryClassBuilderTest >> setUp [
	super setUp.
	fileReference := (FileReference newTempFilePrefix: 'export' suffix: '.exp').
	filename := fileReference fullName.

	writer := HEBinaryReaderWriter new
		stream: (File openForWriteFileNamed:filename);
		yourself.

	reader := HEBinaryReaderWriter new
		stream: (File openForReadFileNamed:filename);
		yourself.

]

{ #category : #initialization }
HEAbstractBinaryClassBuilderTest >> tearDown [
	writer close.
	reader close.
	fileReference delete.
]

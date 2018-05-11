"
I'm an object in charge of import source files.  I know a format object that knows how to parse the files, and I import the parsed results into the image.

I handle doIts normally, but some special cases like class organization, class comment or methods are handled via a double dispatch (See my method extensions for that).

=-=-=- How to use me -=-=-=

If you want to fileIn the code -aka compile/evaluate it:

CodeImporter evaluateReadStream: '2+2!' readStream.

or

CodeImporter evaluateString: '2+2!'

or

CodeImporter evaluateFileNamed: 'something.st'

or

CodeImporter evaluateFileStream: (FileStream readOnlyFileNamed: 'something.st')

Now, you can also generate a model of code declarations inside a file by just creating an instance:

CodeImporter fileStream: (FileStream readOnlyFileNamed: 'something.st').

And then query me sending the message

#codeDeclarations

Instances of me can be created through 
#fromString:
#readStream:
#fileNamed:
#fileStream:
"
Class {
	#name : #CodeImporter,
	#superclass : #Object,
	#instVars : [
		'readStream',
		'codeDeclarations',
		'parserClass',
		'requestor',
		'logSource'
	],
	#category : #'CodeImport-Base'
}

{ #category : #'instance creation' }
CodeImporter class >> chunksFromFileNamed: aFileName [
	^ (self fileNamed: aFileName)
			parseChunks;
			codeDeclarations.

]

{ #category : #'instance creation' }
CodeImporter class >> chunksFromStream: aStream [
	^ (self fileStream: aStream)
			parseChunks;
			codeDeclarations.

]

{ #category : #evaluating }
CodeImporter class >> evaluateFileNamed: aFileName [
	^(self fileNamed: aFileName) evaluateDeclarations
]

{ #category : #evaluating }
CodeImporter class >> evaluateFileStream: aFileStream [
	^(self fileStream: aFileStream) evaluateDeclarations
]

{ #category : #evaluating }
CodeImporter class >> evaluateReadStream: aReadStream [
	^(self readStream: aReadStream) evaluateDeclarations
]

{ #category : #evaluating }
CodeImporter class >> evaluateString: aString [
	^(self fromString: aString) evaluateDeclarations
]

{ #category : #'instance creation' }
CodeImporter class >> fileNamed: aFileName [
	^ self new file: (FileStream readOnlyFileNamed: aFileName).

]

{ #category : #'instance creation' }
CodeImporter class >> fileStream: aFileStream [
	^self new
		file: aFileStream;
		yourself
]

{ #category : #'instance creation' }
CodeImporter class >> fromString: aByteString [ 
	^self readStream: aByteString readStream
]

{ #category : #'instance creation' }
CodeImporter class >> readStream: aReadStream [
	^self new
		readStream: aReadStream;
		yourself
]

{ #category : #accessing }
CodeImporter >> codeDeclarations [
	^codeDeclarations
]

{ #category : #'public access' }
CodeImporter >> evaluate [
	" stripped down version of evaluateDeclarations"
	| value |
	self codeDeclarations do: [ :declaration |
		requestor ifNotNil: [
			requestor contents: declaration contents ].
		value := declaration importFor: requestor logSource: logSource  ].
	self flushChangesFile.
	
	^value
]

{ #category : #evaluating }
CodeImporter >> evaluateDeclarations [
	"Evaluates the declarations from the text in the file and answers the last result"

	self parseChunks.
	^ self evaluate
]

{ #category : #accessing }
CodeImporter >> file: aFileStream [
	self readStream: aFileStream.
	self requestor: FileCompilerRequestor new
]

{ #category : #evaluating }
CodeImporter >> flushChangesFile [
	"Yes, we need to do this in order to flush the file, puaj"
	logSource ifTrue: [
		ChangesLog default logChange: '----End fileIn----'].
]

{ #category : #initialization }
CodeImporter >> initialize [
	codeDeclarations := OrderedCollection new.
	parserClass := ChunkFileFormatParser.
	logSource := true
]

{ #category : #accessing }
CodeImporter >> logSource: aBoolean [

	logSource := aBoolean
]

{ #category : #evaluating }
CodeImporter >> parseChunks [
	^ codeDeclarations := (parserClass for: readStream) parseChunks.
]

{ #category : #accessing }
CodeImporter >> readStream: aReadStream [
	readStream := aReadStream.
]

{ #category : #accessing }
CodeImporter >> requestor [
	^ requestor
]

{ #category : #accessing }
CodeImporter >> requestor: anObject [
	requestor := anObject
]

{ #category : #fileHandling }
CodeImporter >> selectTextConverterForCode [
	self flag: #fix.  "This should not be here probably."
	
	"We need to see the first three bytes in order to see the origin of the file"
	readStream binary.
	((readStream next: 3) = #[ 16rEF 16rBB 16rBF ]) ifTrue: [
		readStream converter: UTF8TextConverter new
	] ifFalse: [
		readStream converter: MacRomanTextConverter new.
	].

	"we restore the position to the start of the file again"
	readStream position: 0.
	
	"We put the file in text mode for the file in"
	readStream text.
]

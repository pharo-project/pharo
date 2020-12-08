"
## Description
I'm an object in charge of import source files.  I know a format object that knows how to parse the files, and I import the parsed results into the image.

I handle doIts normally, but some special cases like class organization, class comment or methods are handled via a double dispatch (See my method extensions for that).
I read code from chunk format. The chunk format is a code format where elements are delimited by exclamation marks. Exclamation mark represents events of compilation (chunk that was being read from input stream is now  evaluated/compiled). 

Each code element is preceded by some kind of annotation that says what kind of element it is / how it should be read.
For example, the annotation:  
``` 
CodeImportTestCaseTestClass #methodsFor: ''some protocol'' #stamp: ''GuillermoPolito 5/2/2012 13:35
```  
says that the thing that follows is a method for the class CodeImportTestCaseTestClass, its protocol, and some other metadata like the last modification stamp.
A chunk can then contain:
- a method
- a class comment
- a ""package organization"" which is like a package definition, 
- an arbitrary ""doIt"" expression. 
- The definition of classes are integrated within arbitrary expressions.

## How to use me

If you want to fileIn the code -aka compile/evaluate it:
`CodeImporter evaluateReadStream: '2+2!' readStream.`  
or  
`CodeImporter evaluateString: '2+2!'`  
or  
`CodeImporter evaluateFileNamed: 'something.st'`  
or  
`CodeImporter evaluateFileStream: (FileStream readOnlyFileNamed: 'something.st')`  

Now, you can also generate a model of code declarations inside a file by just creating an instance:
`CodeImporter fileStream: (FileStream readOnlyFileNamed: 'something.st').`  
And then query me sending the message `#codeDeclarations`

Instances of me can be created through 
`#fromString:`  
`#readStream:`  
`#fileNamed:`  
`#fileStream:`  
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
	^ self new file: (aFileName asFileReference readStream).

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

	readStream text
]

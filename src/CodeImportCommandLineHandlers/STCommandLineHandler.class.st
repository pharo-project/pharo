"
Usage: st [--help] [ --quit ] <FILE>
	--help    list this help message
	--quit    if specified, the image exits without saving after evaluating FILE
	--save    if specified, save the image after evaluating FILE
	--no-source if specified, do not write to the .changes file
	<FILE>    a file containing valid Pharo expressions

Documentation:
The ST command line handler runs Pharo code stored in a file without quiting.

Example:

	pharo Pharo.image st code.st
	
	# By default files ending in .st are recognized and evaluated
	pharo Pharo.image code.st

"
Class {
	#name : #STCommandLineHandler,
	#superclass : #CommandLineHandler,
	#instVars : [
		'sourceFiles'
	],
	#category : #'CodeImportCommandLineHandlers-Base'
}

{ #category : #accessing }
STCommandLineHandler class >> commandName [
	^ 'st'
]

{ #category : #accessing }
STCommandLineHandler class >> description [
	^ 'Loads and executes .st source files'
]

{ #category : #'handler selection' }
STCommandLineHandler class >> isResponsibleFor: aCommandLine [
	"This handler is reponsible only for .st files"
	(aCommandLine hasFileWithExtension: '.st')
		ifTrue: [ ^ true ].
	^ super isResponsibleFor: aCommandLine
]

{ #category : #printing }
STCommandLineHandler class >> printCompilerWarning: aSyntaxErrorNotification [
	| stderr position contents errorLine errorMessage maxLineNumberSize lineNumber |
	
	"format the error"
	position := aSyntaxErrorNotification location.
	contents := aSyntaxErrorNotification errorCode.
	errorLine := contents lineNumberCorrespondingToIndex: position.
	stderr := VTermOutputDriver stderr.
		
	"first gather the error title to be able to underline it properly"
	errorMessage := String streamContents: [ :s|
		s nextPutAll: 'Syntax Error on line '; 
			print: errorLine; nextPutAll: ': '; 
			print: aSyntaxErrorNotification errorMessage].
	
	 stderr red;
		nextPutAll: errorMessage; lf;
		nextPutAll: ('' padLeftTo: errorMessage size with: $=); lf;
		clear.
	
	"print each source line and mark the found syntax error"
	maxLineNumberSize := contents lines size asString size.
	lineNumber := 0.
	contents lineIndicesDo: [:start :endWithoutDelimiters :end |
		lineNumber := lineNumber + 1.
		lineNumber == errorLine ifTrue: [ stderr errorColor ].
		"0 pad the line numbers to the same size"
		stderr 
			nextPutAll: (lineNumber asString padLeftTo: maxLineNumberSize with: $0);
			nextPutAll: ': ';
			nextPutAll: (contents copyFrom: start to: endWithoutDelimiters);
			lf.
			"print the marker under the error line"
		(lineNumber == errorLine)
			ifTrue: [
					stderr nextPutAll:( '_^_' padLeftTo: position - start + maxLineNumberSize + 4);
							 lf;
							 clear]
	]
]

{ #category : #activation }
STCommandLineHandler >> activate [
	self activateHelp
		ifTrue: [  ^ self ].
	
	self selectSourceFiles.
	self installSourceFiles.
	
	self end
]

{ #category : #private }
STCommandLineHandler >> end [
	| quit  |
	
	quit := self commandLine hasOption: 'quit'.
	
	(self commandLine hasOption: 'save') 
		ifTrue: [ Smalltalk snapshot: true andQuit: quit ].
	
	quit
		ifTrue: [ self exitSuccess ].
]

{ #category : #private }
STCommandLineHandler >> handleError: error [	
	
	"for syntax errors we can used the default action"	"otherwise resignal it"
	(error isKindOf: SyntaxErrorNotification)
		ifTrue: [ error defaultAction ]
		ifFalse: [ error pass ]
]

{ #category : #private }
STCommandLineHandler >> handleError: error reference: aReference [
	"Print a header before failing on an errro / syntax notification from the the script loaded by the given request"
	
	"We ignore warnings for now"
	(error isKindOf: OCSemanticWarning)
		ifTrue: [ ^ error pass ].
		
	"spit out a warning if in headless mode, otherwise a debugger will popup"
	Smalltalk isHeadless 
		ifTrue: [ self inform: 'Errors in script loaded from ', aReference name ].
															
	"for syntax errors we can use the default action"															
	(error isKindOf: SyntaxErrorNotification)
		ifTrue: [ ^ error defaultAction ].
	
	(error isKindOf: OCSemanticError)
		ifTrue: [ 
			self class printCompilerWarning: error.
			^ error resume: error defaultAction ].
	
	"otherwise resignal it"
	error pass.
]

{ #category : #installing }
STCommandLineHandler >> handleErrorsDuring: aBlock reference: aReference [
	aBlock
		on: Error, OCSemanticWarning, OCSemanticError
		do: [ :e | self handleError: e reference: aReference ]
]

{ #category : #installing }
STCommandLineHandler >> installSourceFile: aReference [
	"Install the source file given by aFileReference"
	| codeImporter |

	" parse the code given in the source file"
	aReference readStreamDo: [ :binary | | stream |
		stream := ZnCharacterReadStream on: binary encoding: 'utf8'.
		self 
			handleErrorsDuring: [
				self skipShebangFrom: stream.
				codeImporter := CodeImporter fileStream: stream.
				codeImporter logSource: (self hasOption: 'no-source') not.
				codeImporter parseChunks ]
			reference: aReference ].
	
	"evaluate the declarations in a second step to allow for properly closing the source file stream"
	self 
		handleErrorsDuring: [ codeImporter evaluate ]
		reference: aReference.
]

{ #category : #installing }
STCommandLineHandler >> installSourceFiles [
	"Install the previously loaded source files"

	sourceFiles ifNil: [ ^ self ].
	[ sourceFiles do: [ :reference | 
		self installSourceFile: reference ]
	] ensure: [ sourceFiles := nil ].
	
]

{ #category : #loading }
STCommandLineHandler >> loadSourceFiles: anArray [
	"Load all the source files in the given array."

	sourceFiles := anArray collect: [ :each | File named: each ].

]

{ #category : #activation }
STCommandLineHandler >> selectSourceFiles [
	self loadSourceFiles: (self commandLine allFilesWithExtension: '.st').
]

{ #category : #installing }
STCommandLineHandler >> skipShebangFrom: readStream [
	"Skip the shebang sequnce #! at the beginning of a bash file"
	
	readStream wrappedStream position = 0
		ifFalse: [  ^ self "#! can only be skipped at the beginning" ].
	
	(readStream wrappedStream next: 2) = '#!'
		ifFalse: [ 
			readStream wrappedStream position: 0.
			^ self ].
	
	"here we found the shebang, so skip the first line"
	^ readStream nextLine.
	
]

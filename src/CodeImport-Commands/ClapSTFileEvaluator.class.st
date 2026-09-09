"
Reads smalltalk code from a file and evaluates it.

Example:

pharo Pharo.image st code.st

*By default files ending in .st are recognized and evaluated*
pharo Pharo.image code.st
"
Class {
	#name : 'ClapSTFileEvaluator',
	#superclass : 'ClapCodeEvaluator',
	#category : 'CodeImport-Commands',
	#package : 'CodeImport-Commands'
}

{ #category : 'command line' }
ClapSTFileEvaluator class >> commandSpecification [
	<commandline>
	| spec |
	spec := (ClapCommandSpec id: #st)
		commandClass: self;
		description: 'The ST command evaluates Pharo code stored in a .st file ';
		addHelp;
		addFlag: #'no-source' description: 'if specified, do not write to the .changes file';
		addFlag: #'quit' description: 'quit the image after file evaluation (default is to keep the image running)';
		addPositional: #FILE spec: [ :positional |
			positional
				description: 'The Smalltalk file to evaluate (must have .st extension)';
				multiple: true ];
		yourself.
		
	self 
		addSaveFlagTo: spec;
		addRenameOptionTo: spec.
	^ spec
]

{ #category : 'accessing' }
ClapSTFileEvaluator >> allStFiles [

	^ (self positional: #FILE ifAbsent: [ #() ]) 
		select: [ :arg | arg endsWith: '.st' ]
]

{ #category : 'execution' }
ClapSTFileEvaluator >> evaluate [
	
	"polymorphism with ST file evaluator"
	self evaluateStFiles
]

{ #category : 'execution' }
ClapSTFileEvaluator >> evaluateStFile: aFile [

	aFile readStreamDo: [ :binary |
			| stream |
			stream := ZnCharacterReadStream on: binary encoding: 'utf8'.
			[ self evaluateStStream: stream ]
				on: Error
				do: [ :e | self handleError: e reference: aFile ] ]
]

{ #category : 'execution' }
ClapSTFileEvaluator >> evaluateStFiles [

	self allStFiles do: [ :fileName |
		self evaluateStFile: (File named: fileName) ]
]

{ #category : 'execution' }
ClapSTFileEvaluator >> evaluateStStream: aStream [

	self skipShebangFrom: aStream.
	(CodeImporter fileStream: aStream)
		logSource: self shouldLogSource;
		parseChunks;
		evaluate
]

{ #category : 'asserting' }
ClapSTFileEvaluator >> shouldLogEvaluationResult [

	^ false
]

{ #category : 'accessing' }
ClapSTFileEvaluator >> shouldLogSource [

	^ (self hasFlag: #'no-source') not
]

{ #category : 'testing' }
ClapSTFileEvaluator >> shouldQuit [
	^ self hasFlag: #quit
]

{ #category : 'execution' }
ClapSTFileEvaluator >> skipShebangFrom: readStream [
	"Skip the shebang sequnce #! at the beginning of a file"

	readStream wrappedStream position = 0
		ifFalse: [  ^ self "#! can only be skipped at the beginning" ].

	(readStream wrappedStream next: 2) = '#!'
		ifFalse: [
			readStream wrappedStream position: 0.
			^ self ].

	"here we found the shebang, so skip the first line"
	^ readStream nextLine
]

{ #category : 'validating' }
ClapSTFileEvaluator >> validateParameters [
	self allStFiles ifEmpty: [ ^ self context exitFailure: 'Missing st file as argument' ]
]

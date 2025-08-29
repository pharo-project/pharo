"
I evaluate code passed via standard input or as arguments from the command line.
"
Class {
	#name : 'ClapCodeEvaluator',
	#superclass : 'ClapPharoApplication',
	#category : 'Clap-Commands-Pharo',
	#package : 'Clap-Commands-Pharo'
}

{ #category : 'command line' }
ClapCodeEvaluator class >> evaluate [
	<commandline>
	^ (ClapCommandSpec id: #evaluate)
		aliases: #('eval');
		commandClass: self;
		description: 'Print the result of a Pharo expression';
		addHelp;
		addFlag: #save description: 'Save the image after evaluation';
		addFlag: #keepAlive description: 'Keep image running';
		addPositional: #EXPR spec: [ :positional |
			positional
				description: 'The expression to evaluate, joining successive arguments with spaces (if omitted, read the expression from stdin)';
				multiple: true;
				defaultValue: [ :match | "in the absence of an explicit argument, read from standard input"
					Array with: match context stdin upToEnd ] ];
		yourself
]

{ #category : 'execution' }
ClapCodeEvaluator >> execute [
	| result |
	result := Smalltalk compiler evaluate: self source.
	
	self context hasSessionChanged "we might be waking up after a #save:andQuit:"
		ifFalse: [ self outputStreamDo: [ :out | out print: result; lf ] ]
]

{ #category : 'error handling' }
ClapCodeEvaluator >> handleError: error [
	"for syntax errors we print more detailed error informations"

	(error isKindOf: OCCodeError) ifTrue: [
		self class printCodeError: error on: (VTermOutputDriver on: context stderr) ].

	error pass
]

{ #category : 'error handling' }
ClapCodeEvaluator >> handleError: error reference: aReference [
	"Print a header before failing on an errro / syntax notification from the the script loaded by the given request"

	"Display CodeError nicely"

	(error isKindOf: OCCodeError) ifTrue: [
			self class
				printCodeError: error
				on: (VTermOutputDriver on: context stderr) ].

	"spit out a warning if in headless mode, otherwise a debugger will popup"
	Smalltalk isInteractive ifFalse: [
		self inform: 'Errors in script loaded from ' , aReference name ].

	error pass
]

{ #category : 'error handling' }
ClapCodeEvaluator >> handleErrorsDuring: aBlock reference: aReference [
	aBlock
		on: Error
		do: [ :e | self handleError: e reference: aReference ]
]

{ #category : 'accessing' }
ClapCodeEvaluator >> source [
	^ String space join: (self positional: #EXPR)
]

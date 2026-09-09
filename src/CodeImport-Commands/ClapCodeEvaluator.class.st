"
I evaluate code passed via standard input or as arguments from the command line.
"
Class {
	#name : 'ClapCodeEvaluator',
	#superclass : 'ClapPharoApplication',
	#instVars : [
		'expression'
	],
	#category : 'CodeImport-Commands',
	#package : 'CodeImport-Commands'
}

{ #category : 'accessing' }
ClapCodeEvaluator class >> commandDescription [ 

	^ 'This command either evaluates the arguments passed to the image:
	pharo Pharo.image eval  1 + 2
	
or it can read directly from stdin:

	echo "1+2" | $PHARO_VM my.image eval

Important: don''t manually save the image at the end of the expression by calling something like ''Smalltalk snapshot: true andSave: true''! Instead, use the safer --save option.'
]

{ #category : 'command line' }
ClapCodeEvaluator class >> commandSpecification [
	<commandline>
	| spec |
	spec := (ClapCommandSpec id: #evaluate)
		aliases: #('eval');
		commandClass: self;
		description: 'A command that evaluates a Pharo expression, outputs the evaluated result and quits the image.';
		longDescription: self commandDescription;
		addHelp;
		addPositional: #EXPR spec: [ :positional |
			positional
				description: 'The Pharo expression to evaluate, joining successive arguments with spaces (if omitted, read the expression from stdin)';
				multiple: true;
				defaultValue: [ :match | "in the absence of an explicit argument, read from standard input"
					Array with: match context stdin upToEnd ] ];
		yourself.
		
	self 
		addSaveFlagTo: spec;
		addKeepAliveOptionTo: spec;
		addNoQuitOptionTo: spec;
		addRenameOptionTo: spec.
	^ spec
]

{ #category : 'printing' }
ClapCodeEvaluator class >> printCodeError: aCodeError on: aVTermOutputDriver [
	| position contents errorLine errorMessage maxLineNumberSize lineNumber |

	"format the error"
	position := aCodeError position.
	contents := aCodeError sourceCode.
	errorLine := contents lineNumberCorrespondingToIndex: position.

	"first gather the error title to be able to underline it properly"
	errorMessage := String streamContents: [ :s|
		s nextPutAll: 'Syntax Error on line ';
			print: errorLine; nextPutAll: ': ';
			print: aCodeError messageText].

	 aVTermOutputDriver red;
		nextPutAll: errorMessage; lf;
		nextPutAll: ('' padLeftTo: errorMessage size with: $=); lf;
		clear.

	"print each source line and mark the found syntax error"
	maxLineNumberSize := contents lines size asString size.
	lineNumber := 0.
	contents lineIndicesDo: [:start :endWithoutDelimiters :end |
		lineNumber := lineNumber + 1.
		lineNumber == errorLine ifTrue: [ aVTermOutputDriver errorColor ].
		"0 pad the line numbers to the same size"
		aVTermOutputDriver
			nextPutAll: (lineNumber asString padLeftTo: maxLineNumberSize with: $0);
			nextPutAll: ': ';
			nextPutAll: (contents copyFrom: start to: endWithoutDelimiters);
			lf.
			"print the marker under the error line"
		(lineNumber == errorLine)
			ifTrue: [
					aVTermOutputDriver nextPutAll:( '_^_' padLeftTo: position - start + maxLineNumberSize + 4);
							 lf;
							 clear]
	]
]

{ #category : 'execution' }
ClapCodeEvaluator >> evaluate [
	
	"polymorphism with ST file evaluator"

	^ self evaluateExpression
]

{ #category : 'execution' }
ClapCodeEvaluator >> evaluateExpression [
	
	^ [ OpalEvaluator new evaluate: self source ]
		on: Error
		do: [ :error | self handleError: error ]
]

{ #category : 'execution' }
ClapCodeEvaluator >> execute [
	| result |
	
	self validateParameters.
	self prepareImageAndExit.
	
	result := self evaluate.
	self saveSessionIfAsked.
	
	self shouldLogEvaluationResult
		ifTrue: [ self outputStreamDo: [ :out | out print: result; lf ] ]
]

{ #category : 'accessing' }
ClapCodeEvaluator >> expression [

	^ expression ifNil: [ expression := self positional: #EXPR ]
]

{ #category : 'private - accessing' }
ClapCodeEvaluator >> file [

	^ self expression first asFileReference
]

{ #category : 'error handling' }
ClapCodeEvaluator >> handleError: error [
	"for syntax errors we print more detailed error informations"

	self handleError: error reference: nil
]

{ #category : 'error handling' }
ClapCodeEvaluator >> handleError: error reference: aReference [
	"Print a header before failing on an error / syntax notification from the the script loaded by the given request"

	"Display CodeError nicely"

	(error isKindOf: OCCodeError) ifTrue: [
			self class
				printCodeError: error
				on: (VTermOutputDriver on: context stderr) ].

	"put out a warning if in headless mode, otherwise a debugger will popup"
	(aReference isNotNil and: [ Smalltalk isInteractive not ]) 
		ifTrue: [ InformativeNotification signal: 'Errors in script loaded from ' , aReference name ].

	error pass
]

{ #category : 'private - testing' }
ClapCodeEvaluator >> hasOneArgument [

	^ self expression size = 1
]

{ #category : 'testing' }
ClapCodeEvaluator >> isEvaluatingFile [

	"Do not use FileReference here since eval is used in Pharo boostrap and when FileSystem package not yet loaded"
	^ self expression size = 1 and: [ self expression first endsWith: '.st' ]
]

{ #category : 'asserting' }
ClapCodeEvaluator >> shouldLogEvaluationResult [

	"we might be waking up after a #save:andQuit:"
	^ self context hasSessionChanged not
]

{ #category : 'accessing' }
ClapCodeEvaluator >> source [
	^ self isEvaluatingFile 
		ifTrue: [ self file contents ]
		ifFalse: [ String space join: self expression ] 
]

{ #category : 'validating' }
ClapCodeEvaluator >> validateParameters [

	"nothing to do"
]

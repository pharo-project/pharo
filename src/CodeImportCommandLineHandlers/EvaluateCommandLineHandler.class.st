"
Usage: eval [--help] [--save] [ --no-quit ] <smalltalk expression>
	--help                   list this help message
	--save                   save the image after evaluation of the expression
	--no-quit                if specified, the image continues runing after
	                         evaluating the <smalltalk expression>
	<smallltalk expression>  a valid Smalltalk expression which is evaluated and 
	                         the result is printed on stdout

Documentation:
A CommandLineHandler that reads a string from the command line, outputs the evaluated result and quits the image. 

This handler either evaluates the arguments passed to the image:
	pharo Pharo.image eval  1 + 2
	
or it can read directly from stdin:

	echo ""1+2"" | $PHARO_VM my.image eval

Important: don't manually save the image at the end of the expression by calling something like 'Smalltalk snapshot: true andSave: true'! Instead, use the safer --save option.
"
Class {
	#name : #EvaluateCommandLineHandler,
	#superclass : #STCommandLineHandler,
	#category : #'CodeImportCommandLineHandlers-Base'
}

{ #category : #accessing }
EvaluateCommandLineHandler class >> commandName [
	^ 'eval'
]

{ #category : #accessing }
EvaluateCommandLineHandler class >> description [
	^ 'Directly evaluates passed in one line scripts'
]

{ #category : #'handler selection' }
EvaluateCommandLineHandler class >> isResponsibleFor: commandLineArguments [
	"directly handle top-level -e and --evaluate options"
	commandLineArguments withFirstArgument: [ :arg| 
		(#('-e' '--evaluate') includes: arg)
			ifTrue: [ ^ true ]].
	
	^ commandLineArguments includesSubCommand: self commandName
]

{ #category : #activation }
EvaluateCommandLineHandler >> activate [
	self activateHelp
		ifTrue: [  ^ self ].
		
	self arguments 
		ifEmpty: [ ^ self evaluateStdIn ].
		
	self evaluateArguments.
	
	(self hasOption: 'save')
		ifTrue: [ 
			^ Smalltalk 
				snapshot: true 
				andQuit: (self hasOption: 'no-quit') not ].
	
	(self hasOption: 'no-quit')
		ifFalse: [ self quit ]
]

{ #category : #activation }
EvaluateCommandLineHandler >> argumentString [
	| arguments skipArguments |
	
	"Now this is ugly... Coral would be nice here! Basically we strip skipArguments if, and only if they appear at the beginning of arguments"
	skipArguments := #( '--no-quit' '--save' '-e' '--evaluate').

	arguments := (skipArguments includes: self arguments first)
		ifFalse: [ self arguments ]
		ifTrue: [
			(self arguments size >= 2 and: [ skipArguments includes: self arguments second ])
				ifTrue: [ self arguments allButFirst: 2 ]
				ifFalse: [ self arguments allButFirst ]].
		
	^ arguments joinUsing: Character space.
]

{ #category : #commands }
EvaluateCommandLineHandler >> evaluate: aStream [
	| result |
	[ 
	result := Smalltalk compiler evaluate: aStream.
	self hasSessionChanged
		ifFalse: [ 
			self stdout
				print: result;
				lf ] ]
		on: Error
		do: [ :error | self handleError: error ]
]

{ #category : #activation }
EvaluateCommandLineHandler >> evaluateArguments [
	| argumentString |
	argumentString :=  self argumentString.
	argumentString ifEmpty: [ ^ self ].
	self evaluate: argumentString
]

{ #category : #activation }
EvaluateCommandLineHandler >> evaluateStdIn [
	| stdin |
	stdin := FileStream stdin ifNil: [ ^ self ].
	stdin atEnd  ifTrue: [ ^ self ].
	
	self flag: #todo. "the whole compilation machinery does not work with stdin directly, hence we load everything a buffer first"
	self evaluate: (String streamContents: [ :s|
		[ stdin atEnd ] whileFalse: [ 
			stdin next ifNotNil: [ :char|
			s nextPut: char ]]])
]

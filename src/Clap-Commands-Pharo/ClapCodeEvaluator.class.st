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

{ #category : 'accessing' }
ClapCodeEvaluator >> source [
	^ String space join: (self positional: #EXPR)
]

"
I evaluate code passed via standard input or as arguments from the command line.
"
Class {
	#name : #ClapCodeEvaluator,
	#superclass : #Object,
	#instVars : [
		'arguments'
	],
	#category : #'Clap-Commands-Pharo'
}

{ #category : #'command line' }
ClapCodeEvaluator class >> evaluate [
	<commandline>
	^ (ClapCommand withName: 'evaluate')
		aliases: #('eval');
		description: 'Print the result of a Pharo expression';
		add: ClapFlag forHelp;
		add: ((ClapFlag withName: 'save')
			description: 'Save the image after evaluation');
		add: ((ClapFlag withName: 'no-quit')
			description: 'Keep image running');
		add:	((ClapPositional withName: 'EXPR')
			description: 'The expression to evaluate, joining successive arguments with spaces (if omitted, read the expression from stdin)';
			multiple: true;
			meaning: [ :match | "let's copy all the bytes, twice, because the VM assumes some 8-bit encoding…"
				String space join: (match allOccurrencesCollect: #word) ];
			implicitMeaning: [ :match | "in the absence of an explicit argument, read from standard input"
				match context stdin upToEnd ]);
		meaning: [ :args | 
			args	atName: 'help' ifFound: [ :help | help value; exitSuccess ].
			(self with: args) execute ]
]

{ #category : #'instance creation' }
ClapCodeEvaluator class >> with: arguments [
	^ self new
		setArguments: arguments;
		yourself
]

{ #category : #execution }
ClapCodeEvaluator >> context [
	^ arguments context
]

{ #category : #execution }
ClapCodeEvaluator >> execute [
	| result |
	result := Smalltalk compiler evaluate: self source.
	
	self context hasSessionChanged "we might be waking up after a #save:andQuit:"
		ifFalse: [ self outputStreamDo: [ :out | out print: result; lf ] ]

]

{ #category : #execution }
ClapCodeEvaluator >> outputStreamDo: aBlock [
	aBlock value: self context stdout
]

{ #category : #initialization }
ClapCodeEvaluator >> setArguments: args [
	arguments := args
]

{ #category : #initialization }
ClapCodeEvaluator >> source [
	^ (arguments atName: 'EXPR') value
]

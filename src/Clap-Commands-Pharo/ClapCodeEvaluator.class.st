"
I evaluate code passed via standard input or as arguments from the command line.
"
Class {
	#name : #ClapCodeEvaluator,
	#superclass : #ClapPharoApplication,
	#category : #'Clap-Commands-Pharo'
}

{ #category : #'command line' }
ClapCodeEvaluator class >> evaluate [
	<commandline>
	^ (ClapCommand id: #evaluate)
		aliases: #('eval');
		description: 'Print the result of a Pharo expression';
		add: ClapFlag forHelp;
		add: ((ClapFlag id: #save)
			description: 'Save the image after evaluation');
		add: ((ClapFlag id: #keepAlive)
			description: 'Keep image running');
		add:	((ClapPositional id: #EXPR)
			description: 'The expression to evaluate, joining successive arguments with spaces (if omitted, read the expression from stdin)';
			multiple: true;
			meaning: [ :match | "let's copy all the bytes, twice, because the VM assumes some 8-bit encoding…"
				String space join: (match allOccurrencesCollect: #word) ];
			implicitMeaning: [ :match | "in the absence of an explicit argument, read from standard input"
				match context stdin upToEnd ]);
		meaning: [ :args | 
			args	at: #helpFlag ifPresent: [ :help | help value; exitSuccess ].
			(self with: args) execute ]
]

{ #category : #execution }
ClapCodeEvaluator >> execute [
	| result |
	result := Smalltalk compiler evaluate: self source.
	
	self context hasSessionChanged "we might be waking up after a #save:andQuit:"
		ifFalse: [ self outputStreamDo: [ :out | out print: result; lf ] ]

]

{ #category : #accessing }
ClapCodeEvaluator >> source [
	^ (arguments at: #EXPR) value
]

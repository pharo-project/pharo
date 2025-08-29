"
Performs a message on a given class or global object. This command can be used to execute some code if a compiler is not loaded in the image. All message arguments are strings.

Examples:
pharo Pharo.image bootstrap perform Smalltalk garbageCollect --save
"
Class {
	#name : 'ClapPharoPerformCommand',
	#superclass : 'ClapPharoBootstrapCommand',
	#category : 'PerformMessageCommandLineHandler',
	#package : 'PerformMessageCommandLineHandler'
}

{ #category : 'command line' }
ClapPharoPerformCommand class >> commandSpecification [ 

	| command |
	command := (ClapCommandSpec id: #perform)
		  description: 'Performs a message on a given class or global object. This command-line handler can be used to execute some code if a compiler is not loaded in the image. All message arguments are strings.';
		  commandClass: self;
		  addHelp;
		  addPositional: #className description: 'A class name or a global name. Receiver of the message (e.g. Smalltalk)';
		  addPositional: #selector description: 'The message selector to be perfomed';
		  addPositional: #arguments spec: [ :positional |
			positional
				description: 'The Pharo expression to evaluate, joining successive arguments with spaces (if omitted, read the expression from stdin)';
				multiple: true ];
			yourself.
		
	self addSaveFlagTo: command.
	^ command 
]

{ #category : 'accessing' }
ClapPharoPerformCommand >> arguments [

	^ self positional: #arguments 
		ifPresent: [ :args | args asArray ]
		ifAbsent: [ #(  ) ]
]

{ #category : 'execution' }
ClapPharoPerformCommand >> execute [ 

	| result |
	super execute.

	self validate.
	result := self targetClass perform: self selector withArguments: self arguments. 
	self outputStreamDo: [ :out | out print: result; lf ].
	
	self saveSessionIfAsked.
	^ result
]

{ #category : 'accessing' }
ClapPharoPerformCommand >> selector [ 

	^ (self positional: #selector) asSymbol
]

{ #category : 'accessing' }
ClapPharoPerformCommand >> targetClass [

	^ Smalltalk globals
		  at: self targetClassName asSymbol
		  ifAbsent: [ self context exitFailure: 'Unknown class' ]
]

{ #category : 'accessing' }
ClapPharoPerformCommand >> targetClassName [ 

	^ self positional: #className
]

{ #category : 'accessing' }
ClapPharoPerformCommand >> validate [

	[ self targetClassName . self selector ]
	on: NotFound 
	do: [ self context exitFailure: 'Missing className or selector' ]
]

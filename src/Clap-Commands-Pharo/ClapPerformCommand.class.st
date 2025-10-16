"
I define the perform command and delegate implementation to `PerformMessageCommandLineHandler`.
"
Class {
	#name : 'ClapPerformCommand',
	#superclass : 'ClapPharoApplication',
	#category : 'Clap-Commands-Pharo',
	#package : 'Clap-Commands-Pharo'
}

{ #category : 'command line' }
ClapPerformCommand class >> commandSpecification [

	<commandline>
	| spec |
	spec := (ClapCommandSpec id: #perform)
		        description:
			        'Performs a message on a given class or global object. This command can be used to execute some code if a compiler is not loaded in the image. All message arguments are strings.' , String lf, 
						'Example: pharo Pharo.image perform Smalltalk garbageCollect --save';
		        commandClass: self;
		        addHelp;
		        addPositional: #global description: 'A class name or a global name. Receiver of the message (e.g. Smalltalk)';
		        addPositional: #messageSelector description: 'The message selector to be perfomed';
		        addPositional: #arguments spec: [ :positional |
				        positional
					        description:
						        'arguments to be passes to the message selector. All arguments after the special argument -- will be wrapped into an array and passed as a single argument.';
					        multiple: true ];
		        yourself.

	self
		addSaveFlagTo: spec;
		addNoQuitOptionTo: spec.
	spec
      addFlag: #'as-array'
   			description: 'Following arguments will be wrapped into an array and passed as a single argument. Should be used after <arguments>'
      		positionalSpec: [ :positional | positional identifier: #'array-argument'; multiple: true ].
	^ spec
]

{ #category : 'accessing' }
ClapPerformCommand >> allArguments [

	^ context arguments copyWithoutFirst
]

{ #category : 'execution' }
ClapPerformCommand >> execute [ 

	| command |
	
	super execute.
	command := PerformMessageCommandLineHandler commandLine: (CommandLineArguments withArguments: self allArguments).
	command execute.
	self saveSessionIfAsked.
]

{ #category : 'accessing' }
ClapPerformCommand >> messageArguments [
	
	^ self positional: #arguments
]

{ #category : 'accessing' }
ClapPerformCommand >> messageArgumentsAsArray [
	
	^ self positional: #'as-array'
]

{ #category : 'accessing' }
ClapPerformCommand >> messageSelector [
	
	^ self positional: #messageSelector
]

{ #category : 'accessing' }
ClapPerformCommand >> targetClass [
	
	^ self positional: #global
]

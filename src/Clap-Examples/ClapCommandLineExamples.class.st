"
Examples and demos of Clap commands (see class side, protocol commandline). Each of those methods builds and returns a command specification, which you can then run or just match against an actual invocation (an array of strings, as would be passed by a shell).

Arbitrary command instances can be run as do-its, but pay attention that text printed on the standard output will go nowhere unless the VM was launched from a terminal window:
	`ClapCommandLineExamples hello activateWith: #('hello' 'Pharo').`
	`ClapCommandLineExamples git activateWith: #('git' 'help' 'remote').`

To inspect the matches without activating the commands, replace #activateWith: with #match:
	`ClapCommandLineExamples hello match: #('hello' 'Pharo').`
	`ClapCommandLineExamples git match: #('git' 'help' 'remote').`

Commands registered in the system (returned from a class-side method with the <commandline> pragma) can also be invoked from the host system's command line:
```
	$PHARO_VM $IMAGE clap hello Pharo
```
"
Class {
	#name : 'ClapCommandLineExamples',
	#superclass : 'ClapApplication',
	#instVars : [
		'outStream'
	],
	#category : 'Clap-Examples',
	#package : 'Clap-Examples'
}

{ #category : 'command line' }
ClapCommandLineExamples class >> git [
	"A dummy command (no behavior besides help) to demonstrate nested subcommand and as a test subject for the documenter.
	<commandline> pragma omitted, because doesn't really make sense to make it available outside the image."
	^ (ClapCommandSpec id: 'git')
		description: 'The stupid content tracker';
		commandClass: self;
		add: ClapCommandSpec forHelp;
		add: ((ClapCommandSpec id: 'add')
			description: 'Add file contents to the index';
			commandClass: ClapGitAddCommandExample);
		add: ((ClapCommandSpec id: 'branch')
			description: 'List, create, or delete branches');
		add: ((ClapCommandSpec id: 'clone')
			description: 'Clone a repository into a new directory');
		add: ((ClapCommandSpec id: 'commit')
			description: 'Record changes to the repository');
		add: ((ClapCommandSpec id: 'init')
			description: 'Create an empty Git repository or reinitialize an existing one');
		add: ((ClapCommandSpec id: 'remote')
			description: 'Manage set of tracked repositories';
			add: (ClapCommandSpec id: 'add');
			add: (ClapCommandSpec id: 'remove');
			add: (ClapCommandSpec id: 'set-url');
			yourself);
		add: ((ClapCommandSpec id: 'status')
			description: 'Show the working tree status');
		yourself
]

{ #category : 'command line' }
ClapCommandLineExamples class >> hello [
	"The usual Hello-World example, demonstrating a Clap command with a couple features."
	<script>
	<commandline>
	^ (ClapCommandSpec id: #hello)
		description: 'Provides greetings';
		commandClass: self;
		addHelp;
		addFlag: #whisper description: 'Greet discretely';
		addFlag: #shout description: 'Greet loudly';
		addFlag: #language 
			description: 'Select language of greeting' 
			positionalSpec: [ :positional |
				positional
					symbol;
					defaultValue: [ :arg :app | app defaultLanguage ] ];
		addPositional: #who spec: [ :positional |
			positional
				description: 'Recipient of the greetings';
				multiple: true;
				defaultValue: [ :arg :app | { app defaultRecipient } ] ];
		yourself
]

{ #category : 'accessing' }
ClapCommandLineExamples >> defaultLanguage [
	^ #en
]

{ #category : 'accessing' }
ClapCommandLineExamples >> defaultRecipient [
	^ self
		translationFor: self language
		from: self translatedRecipients
]

{ #category : 'running' }
ClapCommandLineExamples >> execute [
	self sayHello
]

{ #category : 'accessing' }
ClapCommandLineExamples >> greeting [
	^ self
		translationFor: self language
		from: self translatedGreetings
]

{ #category : 'accessing' }
ClapCommandLineExamples >> language [
	^ self positional: #language
]

{ #category : 'accessing - private' }
ClapCommandLineExamples >> outStream [
	^ outStream ifNil: [ outStream := VTermOutputDriver on: self context stdout ]
]

{ #category : 'accessing' }
ClapCommandLineExamples >> recipients [
	^ self positional: #who
]

{ #category : 'running' }
ClapCommandLineExamples >> sayHello [
	"arguments validateAll."
	self sayHelloOn: self outStream
]

{ #category : 'running' }
ClapCommandLineExamples >> sayHelloOn: aStream [
	self recipients do: [ :each |
		aStream
			<< (self voice: self greeting , ', ' , each);
			lf ]
]

{ #category : 'accessing' }
ClapCommandLineExamples >> shouting [
	^ self hasFlag: #shout
]

{ #category : 'running' }
ClapCommandLineExamples >> translatedGreetings [
	^ {
		#de -> 'hallo'.
		#en -> 'hello'.
		#eo -> 'saluton'.
		#es -> 'hola'.
		#fr -> 'bonjour'.
		#uk -> 'pryvit'.
		#pt -> 'olá'.
	} asDictionary
]

{ #category : 'running' }
ClapCommandLineExamples >> translatedRecipients [
	^ {
		#de -> 'welt'.
		#en -> 'world'.
		#eo -> 'mondo'.
		#es -> 'mundo'.
		#fr -> 'tout le monde'.
		#uk -> 'svit'.
		#pt -> 'mundo'.
	} asDictionary
]

{ #category : 'running' }
ClapCommandLineExamples >> translationFor: lang from: translations [
	^ translations at: self language
		ifAbsent: [ translations at: self defaultLanguage ]
]

{ #category : 'running' }
ClapCommandLineExamples >> voice: aString [
	self shouting ifTrue: [ ^ aString asUppercase , '!' ].
	self whispering ifTrue: [ ^ '(' , aString asLowercase , ')' ].
	^ aString , '.'
]

{ #category : 'accessing' }
ClapCommandLineExamples >> whispering [
	^ self hasFlag: #whisper
]

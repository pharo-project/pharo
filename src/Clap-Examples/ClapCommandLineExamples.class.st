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
	#name : #ClapCommandLineExamples,
	#superclass : #ClapApplication,
	#instVars : [
		'outStream'
	],
	#category : #'Clap-Examples'
}

{ #category : #'command line' }
ClapCommandLineExamples class >> git [
	"A dummy command (no behavior besides help) to demonstrate nested subcommand and as a test subject for the documenter.
	<commandline> pragma omitted, because doesn't really make sense to make it available outside the image."
	^ (ClapCommand id: 'git')
		description: 'The stupid content tracker';
		add: ClapCommand forHelp;
		add: ((ClapCommand id: 'add')
			description: 'Add file contents to the index');
		add: ((ClapCommand id: 'branch')
			description: 'List, create, or delete branches');
		add: ((ClapCommand id: 'clone')
			description: 'Clone a repository into a new directory');
		add: ((ClapCommand id: 'commit')
			description: 'Record changes to the repository');
		add: ((ClapCommand id: 'init')
			description: 'Create an empty Git repository or reinitialize an existing one');
		add: ((ClapCommand id: 'remote')
			description: 'Manage set of tracked repositories';
			add: (ClapCommand id: 'add');
			add: (ClapCommand id: 'remove');
			add: (ClapCommand id: 'set-url');
			yourself);
		add: ((ClapCommand id: 'status')
			description: 'Show the working tree status');
		yourself
]

{ #category : #'command line' }
ClapCommandLineExamples class >> hello [
	"The usual Hello-World example, demonstrating a Clap command with a couple features."

	<commandline>
	^ (ClapCommand id: #hello)
		description: 'Provides greetings';
		add: ClapFlag forHelp;
		add: ((ClapFlag id: #whisper) description: 'Greet discretely');
		add: ((ClapFlag id: #shout) description: 'Greet loudly');
		add:
			((ClapFlag id: #language)
				description: 'Select language of greeting';
				add: ((ClapPositional id: #langCode)
					meaning: [ :pos | pos word asSymbol ];
					implicitMeaning: [ :arg :app | app defaultLanguage ]);
				meaning: [ :flag :app | (flag at: #langCode) value: app ]);
		add:
			((ClapPositional id: #who)
				description: 'Recipient of the greetings';
				multiple: true;
				implicitMeaning: [ :arg :app | app defaultRecipient ]);
		meaning: [ :args | 
			args at: #helpFlag ifPresent: [ :help | 
				help value; exitSuccess ].
			(self with: args) execute ]
]

{ #category : #'accessing - private' }
ClapCommandLineExamples >> argumentAt: identifier [
	^ (arguments at: identifier) value: self
]

{ #category : #accessing }
ClapCommandLineExamples >> defaultLanguage [
	^ #en
]

{ #category : #accessing }
ClapCommandLineExamples >> defaultRecipient [
	^ self
		translationFor: self language
		from: self translatedRecipients

]

{ #category : #running }
ClapCommandLineExamples >> execute [
	self sayHello

]

{ #category : #accessing }
ClapCommandLineExamples >> greeting [
	^ self
		translationFor: self language
		from: self translatedGreetings

]

{ #category : #accessing }
ClapCommandLineExamples >> language [
	^ self argumentAt: #language
]

{ #category : #'accessing - private' }
ClapCommandLineExamples >> outStream [
	^ outStream ifNil: [ outStream := VTermOutputDriver on: arguments context stdout ]
]

{ #category : #accessing }
ClapCommandLineExamples >> recipients [
	^ (arguments at: #who) allOccurrencesCollect: [ :arg | arg value: self ]
]

{ #category : #running }
ClapCommandLineExamples >> sayHello [
	"arguments validateAll."
	self sayHelloOn: self outStream
]

{ #category : #running }
ClapCommandLineExamples >> sayHelloOn: aStream [
	self recipients do: [ :each |
		aStream
			<< (self voice: self greeting , ', ' , each);
			lf ]
]

{ #category : #accessing }
ClapCommandLineExamples >> shouting [
	^ self argumentAt: #shout
]

{ #category : #running }
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

{ #category : #running }
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

{ #category : #running }
ClapCommandLineExamples >> translationFor: lang from: translations [
	^ translations at: self language
		ifAbsent: [ translations at: self defaultLanguage ]
]

{ #category : #running }
ClapCommandLineExamples >> voice: aString [
	self shouting ifTrue: [ ^ aString asUppercase , '!' ].
	self whispering ifTrue: [ ^ '(' , aString asLowercase , ')' ].
	^ aString , '.'

]

{ #category : #accessing }
ClapCommandLineExamples >> whispering [
	^ self argumentAt: #whisper
]

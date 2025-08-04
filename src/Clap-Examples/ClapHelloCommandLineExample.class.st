"
Hello World example of Clap commands.

Command specification is defined class side and returns a command specification, which you can then run or just match against an actual invocation (an array of strings, as would be passed by a shell).

Arbitrary command instances can be run as do-its, but pay attention that text printed on the standard output will go nowhere unless the VM was launched from a terminal window:
```
ClapHelloCommandLineExample hello activateWith: #('hello' 'Pharo').
```

To inspect the matches without activating the commands, replace #activateWith: with #match:
```
ClapHelloCommandLineExample hello match: #('hello' 'Pharo').
```

Commands registered in the system (returned from a class-side method with the <commandline> pragma) can also be invoked from the host system's command line:
```
	$PHARO_VM $IMAGE clap hello Pharo
```
"
Class {
	#name : 'ClapHelloCommandLineExample',
	#superclass : 'ClapApplication',
	#instVars : [
		'outStream'
	],
	#category : 'Clap-Examples',
	#package : 'Clap-Examples'
}

{ #category : 'command line' }
ClapHelloCommandLineExample class >> hello [
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
ClapHelloCommandLineExample >> defaultLanguage [
	^ #en
]

{ #category : 'accessing' }
ClapHelloCommandLineExample >> defaultRecipient [
	^ self
		translationFor: self language
		from: self translatedRecipients
]

{ #category : 'running' }
ClapHelloCommandLineExample >> execute [
	self sayHello
]

{ #category : 'accessing' }
ClapHelloCommandLineExample >> greeting [
	^ self
		translationFor: self language
		from: self translatedGreetings
]

{ #category : 'testing' }
ClapHelloCommandLineExample >> isShouting [
	^ self hasFlag: #shout
]

{ #category : 'testing' }
ClapHelloCommandLineExample >> isWhispering [
	^ self hasFlag: #whisper
]

{ #category : 'accessing' }
ClapHelloCommandLineExample >> language [
	^ self positional: #language
]

{ #category : 'accessing - private' }
ClapHelloCommandLineExample >> outStream [
	^ outStream ifNil: [ outStream := VTermOutputDriver on: self context stdout ]
]

{ #category : 'accessing' }
ClapHelloCommandLineExample >> recipients [
	^ self positional: #who
]

{ #category : 'running' }
ClapHelloCommandLineExample >> sayHello [
	"arguments validateAll."
	self sayHelloOn: self outStream
]

{ #category : 'running' }
ClapHelloCommandLineExample >> sayHelloOn: aStream [
	self recipients do: [ :each |
		aStream
			<< (self voice: self greeting , ', ' , each);
			lf ]
]

{ #category : 'running' }
ClapHelloCommandLineExample >> translatedGreetings [
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
ClapHelloCommandLineExample >> translatedRecipients [
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
ClapHelloCommandLineExample >> translationFor: lang from: translations [
	^ translations at: self language
		ifAbsent: [ translations at: self defaultLanguage ]
]

{ #category : 'running' }
ClapHelloCommandLineExample >> voice: aString [
	self isShouting ifTrue: [ ^ aString asUppercase , '!' ].
	self isWhispering ifTrue: [ ^ '(' , aString asLowercase , ')' ].
	^ aString , '.'
]

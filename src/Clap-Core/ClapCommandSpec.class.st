"
I represent a command or subcommand specification.

Commands are recognized by a keyword, possibly with aliases for convenience. Besides positionals, commands can have flags, as well as nested (sub)commands.

Subcommands work like a trie, to organize and select the various behaviors of a complex program. At each level in a given invocation, at most one subcommand will be recognized, most often as the last parameter of its parent command.


To create a new instance of this class, you must write this:
	ClapCommand withName: <CommandName>
	
And after, if you want to add a flag:
	addFlag: ClapFlag withName: <FlagName>
	
If you want to add a positional:
	addPositional: ClapPositional withName: <PositionalName>
	
If you want to add a subcommand:
	addSubCommand: <subCommand>
	
Example for the eval command: 
	(ClapCommand withName: 'eval')
		addFlag: ClapFlag withName: 'help';
		addPositional: ClapPositionnal withName: 'smalltalk 		expression'.
"
Class {
	#name : 'ClapCommandSpec',
	#superclass : 'ClapParameterized',
	#instVars : [
		'flags',
		'subcommands',
		'commandClass'
	],
	#category : 'Clap-Core-Specification',
	#package : 'Clap-Core',
	#tag : 'Specification'
}

{ #category : 'predefined commands' }
ClapCommandSpec class >> forHelp [
	^ (self id: #helpCommand)
		description: 'Prints command documentation';
		canonicalName: 'help';

		add: ((ClapPositional id: #topic)
			description: 'The subcommand to document (defaults to the current one)';
			meaning: [ :pos :cmd | cmd subcommandNamed: pos word ifNone: nil ];
			implicitMeaning: [ :pos :cmd | cmd ]);

		meaning: [ :match | | doc parent query topic |
			doc := ClapDocumenter on: match context stdout.
			parent := match parent specification.
			query := match at: #topic.
			topic := query value: parent.
			topic
				ifNil: [ match context exitFailure: 'Unknown subcommand: ' , query word ]
				ifNotNil: [ doc explain: topic ] ]
]

{ #category : 'adding' }
ClapCommandSpec >> addFlag: aFlag [
	flags add: aFlag
]

{ #category : 'api' }
ClapCommandSpec >> addFlag: anIdentifier description: aString [
	| flag |
	
	flag := ClapFlag id: anIdentifier.
	flag description: aString.
	self add: flag.
	^ flag
]

{ #category : 'api' }
ClapCommandSpec >> addFlag: anIdentifier description: aString positionalSpec: aPositionalSpecBlock [
	
	| flag positional |
	
	flag := self addFlag: anIdentifier description: aString.
	positional := ClapPositional id: flag positionalIdentifier.
	aPositionalSpecBlock cull: positional. "apply the positional spec"
	flag add: positional.
	^ flag
	
]

{ #category : 'api' }
ClapCommandSpec >> addFlagWithPositional: anIdentifier description: aString [
	
	^ self addFlag: anIdentifier description: aString positionalSpec: [ "nothing to do" ]
]

{ #category : 'api' }
ClapCommandSpec >> addHelp [

	| flag |
	flag := ClapFlag forHelp.
	self addFlag: flag.
	^ flag
]

{ #category : 'api' }
ClapCommandSpec >> addPositional: anIdentifier description: aString [
	
	| positional |
	
	positional := ClapPositional id: anIdentifier.
	positional description: aString.
	self add: positional.
	^ positional
]

{ #category : 'api' }
ClapCommandSpec >> addPositional: anIdentifier spec: aBlock [

	| positional |
	positional := ClapPositional id: anIdentifier.
	aBlock value: positional.
	self add: positional.
	^ positional
]

{ #category : 'adding' }
ClapCommandSpec >> addSubcommand: aCommand [
	subcommands add: aCommand
]

{ #category : 'adding' }
ClapCommandSpec >> addTo: parentParameter [
	^ parentParameter addSubcommand: self
]

{ #category : 'matching - testing' }
ClapCommandSpec >> canMatchWith: word [
	^ self hasAlias: word
]

{ #category : 'accessing' }
ClapCommandSpec >> commandClass [
	^ commandClass
]

{ #category : 'accessing' }
ClapCommandSpec >> commandClass: aClass [
	commandClass := aClass
]

{ #category : 'accessing' }
ClapCommandSpec >> flags [
	^ flags
]

{ #category : 'initialization' }
ClapCommandSpec >> initialize [
	super initialize.
	subcommands := OrderedCollection new.
	flags := OrderedCollection new.
]

{ #category : 'testing' }
ClapCommandSpec >> isCommand [

	^ true
]

{ #category : 'accessing' }
ClapCommandSpec >> nestedPositionalIds [
	
	^ self nestedPositionals 
		flatCollect: [ :flag | flag positionals collect: [ :positional | positional identifier ] ]
]

{ #category : 'accessing' }
ClapCommandSpec >> nestedPositionals [
	"A Nested positional is a positional nested into a flag, e.g. --lang fr
	where --lang is a flag and fr is a positional inside the flag."
	
	^ self flags select: [ :flag | flag positionals isNotEmpty ]
]

{ #category : 'enumerating' }
ClapCommandSpec >> parametersDo: aBlock [
	self flags do: aBlock.
	self subcommands do: aBlock.
	super parametersDo: aBlock.
]

{ #category : 'accessing' }
ClapCommandSpec >> subcommandNamed: specName ifNone: aBlock [
	^ subcommands
		detect: [ :cmd | cmd hasAlias: specName ]
		ifNone: aBlock
]

{ #category : 'accessing' }
ClapCommandSpec >> subcommands [
	^ subcommands
]

{ #category : 'documenting' }
ClapCommandSpec >> synopsisOn: aStream [
	aStream nextPutAll: self canonicalName
]

{ #category : 'accessing' }
ClapCommandSpec >> valueFor: aMatch with: arg [
	^ aMatch
		matchedSubcommand: [ :sub | sub value: arg ]
		ifNone: [ super valueFor: aMatch with: arg ]
]

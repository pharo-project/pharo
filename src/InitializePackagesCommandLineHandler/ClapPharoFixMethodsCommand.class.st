"
Initializes packages structure from information provided in text files. This is used to initialize
packages structure for bootstrapped images. Do not use it on full Pharo images.

Protocols are provided in file where every method has one line and provides following data separated by  tabs:

•	class name
•	method is on the class side (true/false)
•	method selector
•	protocol name


The <cr> line ending is expected for protocols and packages file.

Protocols file example:
```
ASTReplaceRule	false	searchForTree:replaceWith:	matching
ASTPatternPragmaNode	true	selector:keywordsPositions:arguments:	instance creation
```
Packages file contains list of packages where every package is on own line.

Examples:
```language=bash
pharo Pharo.image initializePackages --protocols=protocols.txt --packages --save
```
"
Class {
	#name : 'ClapPharoFixMethodsCommand',
	#superclass : 'ClapPharoBootstrapCommand',
	#category : 'InitializePackagesCommandLineHandler',
	#package : 'InitializePackagesCommandLineHandler'
}

{ #category : 'command line' }
ClapPharoFixMethodsCommand class >> commandSpecification [ 

	| command |
	command := (ClapCommandSpec id: #fixMethods)
		  description: 'Fix methods created by espell by setting their protocol or package if extension.';
		  commandClass: self;
		  addHelp;
		  addFlagWithPositional: #protocols description: 'Fixes protocols of methods provided in the file given as argument';
		  addFlag: #packages description: 'Fixes packages of extension methods';
			yourself.
		
	self addSaveFlagTo: command.
	^ command 
]

{ #category : 'execution' }
ClapPharoFixMethodsCommand >> execute [

	super execute.

	self
		positional: #protocols
		ifPresent: [ :fileName | PharoBootstrapFixMethodsTool fixMethodsIn: fileName ].

	(self hasFlag: #packages) ifTrue: [ PharoBootstrapFixMethodsTool fixExtensionMethods ].

	self saveSessionIfAsked
]

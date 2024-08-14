"
Usage: initializePackages [ --protocols=<protocols> ] [ --packages=<packages> ]  [ --save ]
	protocols	A name of a file with method protocols specification
	packages	a name of a file with packages list
	
Documentation:
Initializes packages structure from information provided in text files. This is used to initialize 
packages structure for bootstrapped images. Do not use it on full Pharo images.

Protocols are provided in file where every method has one line and provides following data separated by  tabs:
- class name
- method is on the class side (true/false)
- method selector
- protocol name

The <cr> line ending is expected for protocols and packages file.

Protocols file exxample:
RBReplaceRule	false	searchForTree:replaceWith:	matching
ASTPatternPragmaNode	true	selector:keywordsPositions:arguments:	instance creation
			
Packages file contains list of packages where every package is on own line.

Examples:
	pharo Pharo.image initializePackages --protocols=protocols.txt --packages=packages.txt --save
"
Class {
	#name : 'InitializePackagesCommandLineHandler',
	#superclass : 'CommandLineHandler',
	#category : 'InitializePackagesCommandLineHandler',
	#package : 'InitializePackagesCommandLineHandler'
}

{ #category : 'accessing' }
InitializePackagesCommandLineHandler class >> commandName [
	^ 'initializePackages'
]

{ #category : 'accessing' }
InitializePackagesCommandLineHandler class >> description [
	^ 'Initialize packages and protocols from a given lists'
]

{ #category : 'activation' }
InitializePackagesCommandLineHandler >> activate [

	self activateHelp ifTrue: [ ^ self ].

	(self hasOption: 'protocols') ifTrue: [ self initializeProtocolsFrom: (self optionAt: 'protocols') ].

	(self hasOption: 'packages') ifTrue: [ self initializePackages ].

	(self hasOption: 'save') ifTrue: [ Smalltalk snapshot: true andQuit: false ].
	self exitSuccess
]

{ #category : 'activation' }
InitializePackagesCommandLineHandler >> initializePackages [

	PackageOrganizer default basicBootstrapInitialize
]

{ #category : 'activation' }
InitializePackagesCommandLineHandler >> initializeProtocolsFrom: aFileName [

	(File named: aFileName) readStreamDo: [ :binaryStream |
		| stream |
		stream := ZnCharacterReadStream on: binaryStream encoding: 'utf8'.
		[ stream atEnd ] whileFalse: [
			| line items selector class |
			line := stream upTo: Character cr.
			items := line findTokens: String tab.
			class := items second = 'true'
				         ifTrue: [ (Smalltalk classOrTraitNamed: items first) classSide ]
				         ifFalse: [ Smalltalk classOrTraitNamed: items first ].
			selector := items third asSymbol.

			class ifNil: [ self error: 'Could not found class: ' , items first printString , ' for selector: ' , selector printString ].

			(class selectors includes: selector) ifTrue: [ class classify: selector under: items fourth ] ] ]
]

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
	#name : 'PharoBootstrapFixMethodsTool',
	#superclass : 'Object',
	#category : 'PharoBootstrap-Initialization',
	#package : 'PharoBootstrap-Initialization'
}

{ #category : 'fixing' }
PharoBootstrapFixMethodsTool class >> fixExtensionMethods [

	| allBehaviors |
	allBehaviors := Smalltalk allClassesAndTraits.

	"This is a hack because of a weird behavior produced by the bootstrap were the package or tags end up containing classes that are not the same instances than we have in the system dictionary..To be investigated..."
	PackageOrganizer default packages do: [ :package |
		package tags do: [ :tag |
			tag instVarNamed: #classes put: (tag classes collect: [ :class | allBehaviors detect: [ :aClass | aClass name = class name ] ]) ] ].

	self flag: #todo. "This next step should not be needed when we will not use announcements to manage method addition"
	allBehaviors
		do: [ :behavior |
			{
				behavior.
				behavior classSide } do: [ :aBehavior |
				aBehavior extensionProtocols do: [ :protocol |
					(PackageOrganizer default ensurePackageMatching: protocol name allButFirst trimBoth) importProtocol: protocol forClass: aBehavior ] ] ]
]

{ #category : 'private' }
PharoBootstrapFixMethodsTool class >> fixMethod: methodSpecItems [

	| class selector |
	class := Smalltalk classOrTraitNamed: methodSpecItems first.
	methodSpecItems second = 'true' ifTrue: [ class := class classSide ].
	selector := methodSpecItems third asSymbol.

	class ifNil: [
			self error:
				'Could not found class: ' , methodSpecItems first printString
				, ' for selector: ' , selector printString ].

	(class selectors includes: selector) ifTrue: [
		class classify: selector under: methodSpecItems fourth ]
]

{ #category : 'fixing' }
PharoBootstrapFixMethodsTool class >> fixMethodsIn: aFileName [

	(File named: aFileName) readStreamDo: [ :binaryStream |
			| stream |
			stream := ZnCharacterReadStream on: binaryStream encoding: 'utf8'.
			[ stream atEnd ] whileFalse: [
					| line items |
					line := stream upTo: Character cr.
					items := line findTokens: String tab.
					self fixMethod: items ] ]
]

{ #category : 'see class side' }
PharoBootstrapFixMethodsTool >> seeClassSide [ 
]

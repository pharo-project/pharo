"
I run a BaselineOf without installing it. From its Monticello definitions I compile an anonymous subclass of BaselineOf, installed nowhere, and ask Metacello for the version spec it builds on this image (so `for:` blocks are resolved for this platform). I also inspect its methods without running them: what it asks the image, the removed Metacello API it uses, its platform blocks, its minimum Pharo, its replaces.

When I know where the baseline is read from (repository:tag/path), a baseline asking where it is loaded from (packageRepositoryURLForSpec:, or the older packageRepositoryURL) gets that, as when Metacello loads it from there: a project of the same repository then stays at the version read.

requirementsInto: gives an MetacelloMVSBaselineReading the baseline class and its version spec; the reading expands groups from them.
"
Class {
	#name : 'MetacelloMVSBaselineSandbox',
	#superclass : 'Object',
	#instVars : [
		'definitions',
		'repositoryString'
	],
	#category : 'Metacello-MVS',
	#package : 'Metacello-MVS'
}

{ #category : 'instance creation' }
MetacelloMVSBaselineSandbox class >> onDefinitions: aCollection [
	"The Monticello definitions of a BaselineOf class, whatever format they were read from"

	^ self new setDefinitions: aCollection
]

{ #category : 'instance creation' }
MetacelloMVSBaselineSandbox class >> onDefinitions: aCollection repository: aRepositoryString [

	^ (self onDefinitions: aCollection)
		  setRepositoryString: aRepositoryString;
		  yourself
]

{ #category : 'reading' }
MetacelloMVSBaselineSandbox >> anonymousSubclassOf: aClass named: aSymbol with: aBlock [
	"A subclass of aClass named aSymbol, installed nowhere. The class builder looks the name up in the environment it installs into, even for an anonymous class, and would update a class of that name in the image: it builds in an empty environment instead. The class then gets the image's environment back, so that its methods compile against the image's globals"

	| class |
	class := Smalltalk anonymousClassInstaller make: [ :builder |
			         builder
				         installingEnvironment: SystemDictionary new;
				         superclass: aClass;
				         name: aSymbol.
			         aBlock value: builder ].
	class environment: Smalltalk globals.
	^ class
]

{ #category : 'reading' }
MetacelloMVSBaselineSandbox >> baselineClass [
	"An anonymous subclass of BaselineOf with the variables and methods of my definitions, installed nowhere: a class of the same name in the image is neither used nor changed. The extensions the baseline package makes to other classes are left out: compiled here they would land in the wrong class, and in a package of the image"

	| classDefinition baselineClass |
	classDefinition := definitions detect: [ :each | each isClassDefinition ].
	baselineClass := self anonymousSubclassOf: self repositorySuperclass named: classDefinition className with: [ :builder |
			                 builder
				                 slots: (classDefinition instVarNames collect: [ :each | each asSymbol asSlot ]);
				                 classSlots: (classDefinition classInstVarNames collect: [ :each | each asSymbol asSlot ]);
				                 sharedVariablesFromString: (' ' join: classDefinition classVarNames) ].
	definitions do: [ :each |
			(each isMethodDefinition and: [ each className = classDefinition className ]) ifTrue: [
					(each classIsMeta
						 ifTrue: [ baselineClass class ]
						 ifFalse: [ baselineClass ]) compile: each source classified: each category ] ].
	^ baselineClass
]

{ #category : 'reading' }
MetacelloMVSBaselineSandbox >> copiesProjects [
	"Whether the baseline aliases a project with project:copyFrom:with:, a second name for the same requirement"

	self methodASTsDo: [ :ast | (ast sentMessages includes: #project:copyFrom:with:) ifTrue: [ ^ true ] ].
	^ false
]

{ #category : 'reading' }
MetacelloMVSBaselineSandbox >> imageQueries [
	"What the baseline asks the image to decide its dependencies, in its code (not its comments): the globals Smalltalk, SystemVersion, BaselineOfPharo, and the messages classNamed: and environment. Its requirements can then depend on the image that resolves them"

	| found |
	found := Set new.
	self methodASTsDo: [ :ast |
			(ast allChildren select: [ :node | node isVariable ]) do: [ :node |
				(#( #Smalltalk #SystemVersion #BaselineOfPharo ) includes: node name asSymbol) ifTrue: [ found add: node name asString ] ].
			(ast sentMessages select: [ :each |
				 { #classNamed:. #classNamed:ifPresent:. #classNamed:ifPresent:ifAbsent:. #environment } includes: each ]) do: [ :each |
				found add: each asString ] ].
	^ found asSortedCollection asArray
]

{ #category : 'reading' }
MetacelloMVSBaselineSandbox >> messageNodesDo: aBlock [

	self methodASTsDo: [ :ast | ast allChildren do: [ :node | node isMessage ifTrue: [ aBlock value: node ] ] ]
]

{ #category : 'reading' }
MetacelloMVSBaselineSandbox >> methodASTsDo: aBlock [

	definitions do: [ :each | each isMethodDefinition ifTrue: [ aBlock value: (OCParser parseMethod: each source) ] ]
]

{ #category : 'reading' }
MetacelloMVSBaselineSandbox >> platformBlocks [
	"The Pharo-specific attributes of for:do: blocks, such as #'pharo14.x'"

	| found |
	found := Set new.
	self messageNodesDo: [ :node |
			(node selector = #for:do: and: [ node arguments first isLiteralNode ]) ifTrue: [
					| attributes |
					attributes := node arguments first value.
					attributes isSymbol ifTrue: [ attributes := { attributes } ].
					attributes do: [ :each |
							(each isSymbol and: [ (each beginsWith: 'pharo') and: [ each anySatisfy: [ :char | char isDigit ] ] ]) ifTrue: [
								found add: each asString ] ] ] ].
	^ found asSortedCollection asArray
]

{ #category : 'reading' }
MetacelloMVSBaselineSandbox >> replacements [
	"What the baseline replaces with spec replace: 'Bloc' with: 'github://me/Bloc:v3.0.1-fix/src': project name -> repository string, literals only"

	| found |
	found := Dictionary new.
	self messageNodesDo: [ :node |
			(node selector = #replace:with: and: [ node arguments allSatisfy: [ :each | each isLiteralNode and: [ each value isString ] ] ])
				ifTrue: [ found at: node arguments first value put: node arguments last value ] ].
	^ found
]

{ #category : 'reading' }
MetacelloMVSBaselineSandbox >> repositorySuperclass [
	"BaselineOf, or an anonymous subclass of it answering where the baseline is loaded from: where I read it. A superclass, so that a baseline overriding these methods can still call super"

	| superclass |
	repositoryString ifNil: [ ^ BaselineOf ].
	superclass := self anonymousSubclassOf: BaselineOf named: #MetacelloMVSSandboxBaseline with: [ :builder |  ].
	superclass
		compile: 'packageRepositoryURLForSpec: spec ^ ' , repositoryString printString classified: 'mvs';
		compile: 'packageRepositoryURL ^ ' , repositoryString printString classified: 'mvs'.
	^ superclass
]

{ #category : 'reading' }
MetacelloMVSBaselineSandbox >> requirementsInto: aReading [
	"Give aReading the baseline class and its version spec on this platform, and answer the projects a load of the default groups requires, as project specs (see MetacelloMVSBaselineReading>>#referencesFor:into:)"

	aReading baselineClass: self baselineClass.
	^ aReading referencesFor: #(  ) into: aReading
]

{ #category : 'reading' }
MetacelloMVSBaselineSandbox >> setDefinitions: aCollection [

	definitions := aCollection
]

{ #category : 'reading' }
MetacelloMVSBaselineSandbox >> setRepositoryString: aString [

	repositoryString := aString
]

{ #category : 'reading' }
MetacelloMVSBaselineSandbox >> usesRemovedMetacelloAPI [
	"Whether the baseline sends packageRepositoryURL, which Metacello no longer has (packageRepositoryURLForSpec: replaced it) and which only this sandbox still answers"

	self methodASTsDo: [ :ast | (ast sentMessages includes: #packageRepositoryURL) ifTrue: [ ^ true ] ].
	^ false
]

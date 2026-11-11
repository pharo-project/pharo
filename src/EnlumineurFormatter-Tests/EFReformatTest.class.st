"
Tests CompiledMethod>>reformat and reformatAll, which recompile installed methods with the formatter their class's package declares in its manifest.

The generated package's manifest wraps at 20 columns, a width no default formatter uses, so a method formatted with the default cannot pass for one formatted with the package's formatter.
"
Class {
	#name : 'EFReformatTest',
	#superclass : 'TestCase',
	#category : 'EnlumineurFormatter-Tests',
	#package : 'EnlumineurFormatter-Tests'
}

{ #category : 'helpers' }
EFReformatTest >> longMethodNamed: aSelector [
	"Too long for the package's 20 columns, short enough for any default."

	^ aSelector , ' ^ 1 between: 1000000 and: 2000000'
]

{ #category : 'helpers' }
EFReformatTest >> newClassNamed: aName superclass: aClass [

	^ self class classInstaller make: [ :builder |
			  builder
				  name: aName;
				  superclass: aClass;
				  package: self packageNameForTests ]
]

{ #category : 'helpers' }
EFReformatTest >> newClassNamed: aName uses: aTrait [

	^ self class classInstaller make: [ :builder |
			  builder
				  name: aName;
				  traitComposition: aTrait;
				  package: self packageNameForTests ]
]

{ #category : 'helpers' }
EFReformatTest >> newTraitNamed: aName [

	^ self class classInstaller make: [ :builder |
			  builder
				  name: aName;
				  beTrait;
				  package: self packageNameForTests ]
]

{ #category : 'helpers' }
EFReformatTest >> packageNameForTests [

	^ #'Generated-EnlumineurFormatter-Reformat-Test-Package'
]

{ #category : 'running' }
EFReformatTest >> setUp [

	super setUp.
	(self newClassNamed: #ManifestGeneratedEFReformatTestPackage superclass: PackageManifest) class compile:
		'formatter ^ EFFormatter new installNewContext: (EFContext new maxLineLength: 20)'
]

{ #category : 'running' }
EFReformatTest >> tearDown [

	self packageOrganizer removePackage: self packageNameForTests.
	super tearDown
]

{ #category : 'tests' }
EFReformatTest >> testReformatAllFormatsBothSidesAndLeavesTraitMethods [

	| trait class |
	trait := self newTraitNamed: #TEFReformatTestTrait.
	trait compile: (self longMethodNamed: 'traitProbe').
	class := self newClassNamed: #EFReformatTestClass uses: trait.
	class compile: (self longMethodNamed: 'instanceSideProbe').
	class classSide compile: (self longMethodNamed: 'classSideProbe').

	class reformatAll.

	{
		(class >> #instanceSideProbe).
		(class classSide >> #classSideProbe) } do: [ :method |
		self assert: method sourceCode equals: (method ast formattedCodeIn: method methodClass) ].
	self assert: (class >> #traitProbe) isFromTrait.
	self assert: (trait >> #traitProbe) sourceCode equals: (self longMethodNamed: 'traitProbe')
]

{ #category : 'tests' }
EFReformatTest >> testReformatFormatsATraitMethodInItsTrait [
	"A method a class has from a trait belongs to the trait. Recompiling it in the class would override the trait there."

	| trait class |
	trait := self newTraitNamed: #TEFReformatTestTrait.
	trait compile: (self longMethodNamed: 'foo').
	class := self newClassNamed: #EFReformatTestClass uses: trait.

	(class >> #foo) reformat.

	self assert: (class >> #foo) isFromTrait.
	self assert: (trait >> #foo) sourceCode equals: ((trait >> #foo) ast formattedCodeIn: trait)
]

{ #category : 'tests' }
EFReformatTest >> testReformatFormatsWithTheFormatterOfThePackage [
	"Not with the formatter of the package defining the AST nodes."

	| class method |
	class := self newClassNamed: #EFReformatTestClass superclass: Object.
	class compile: (self longMethodNamed: 'foo').

	(class >> #foo) reformat.

	method := class >> #foo.
	self assert: method sourceCode equals: (method ast formattedCodeIn: class).
	self deny: method sourceCode equals: method ast formattedCode
]

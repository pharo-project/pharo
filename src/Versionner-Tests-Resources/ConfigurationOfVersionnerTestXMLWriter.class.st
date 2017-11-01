"
Simple Configuration class used to test Versionner behavior.
This class is a rename of ConfigurationOfXMLWriter-StephaneDucasse.29.

"
Class {
	#name : #ConfigurationOfVersionnerTestXMLWriter,
	#superclass : #Object,
	#instVars : [
		'project'
	],
	#classVars : [
		'LastVersionLoad'
	],
	#category : 'Versionner-Tests-Resources'
}

{ #category : #private }
ConfigurationOfVersionnerTestXMLWriter class >> bootstrapPackage: aString from: aPath [ 
	| repository version |
	repository := MCHttpRepository
				location: aPath
				user: ''
				password: ''.
	repository
		versionReaderForFileNamed: aString , '.mcz'
		do: [:reader | 
			version := reader version.
			version load.
			version workingCopy repositoryGroup addRepository: repository]
]

{ #category : #private }
ConfigurationOfVersionnerTestXMLWriter class >> ensureMetacello [
	"Bootstrap Gofer (if necessary), load latest mcz file for ConfigurationOfMetacello (using old 
	 Gofer API), then load the latest version of Metacello itself."

	Smalltalk at: #MetacelloProject ifAbsent: [ | error |
		"list of repositories to try, in case primary repository is not accessible"
		(Array 
			with: 'http://www.squeaksource.com/MetacelloRepository' 
			with: 'http://seaside.gemstone.com/ss/metacello') do: [:repositoryUrl |
				([
					Smalltalk at: #Gofer ifAbsent: [
					"Current version of Gofer from which to bootstrap - as of 1.0-beta.21"
						self bootstrapPackage: 'Gofer-Core-lr.115' from: repositoryUrl ].
					Smalltalk at: #Gofer ifPresent: [:goferClass | | gofer |
						gofer := goferClass new
							url: repositoryUrl;
							yourself.
						[ gofer addPackage: 'ConfigurationOfMetacello' ] on: Warning do: [:ex | ex resume ].
						gofer load ]]
							on: Error
							do: [ :ex |
								error := ex.
								Transcript 
									cr; 
									show: 'failed ensureMetacello: ';
									show: ex description printString;
									show: '...retrying'.
								"try again"
								ex return: nil ]) ~~ nil
					ifTrue: [
						"load 'default' group of Metacello"
						(Smalltalk at: #ConfigurationOfMetacello) perform: #load.
						^self ]].
		"shouldn't get here unless the load failed ... throw an error"
		self error: 'retry with alternate repository failed: ', error description printString ]
]

{ #category : #testing }
ConfigurationOfVersionnerTestXMLWriter class >> isMetacelloConfig [
	"Answer true and the Metacello tools will operate on you"
	
	^true
]

{ #category : #'metacello tool support' }
ConfigurationOfVersionnerTestXMLWriter class >> lastMetacelloVersionLoad [
	"Answer the last version loaded and the list of packages loaded for that version."

	LastVersionLoad == nil ifTrue: [ LastVersionLoad := nil -> 'default' ].
	^LastVersionLoad
]

{ #category : #loading }
ConfigurationOfVersionnerTestXMLWriter class >> load [
	"self load"

	^self project latestVersion load
]

{ #category : #'metacello tool support' }
ConfigurationOfVersionnerTestXMLWriter class >> metacelloVersion: versionString loads: anArrayOrString [
	"Stash the last version loaded and the list of packages loaded for that version. The list
	 of packages will be used by the tools when doing 'Load Package Version'"

	LastVersionLoad := versionString -> anArrayOrString
]

{ #category : #accessing }
ConfigurationOfVersionnerTestXMLWriter class >> project [

	^self new project
]

{ #category : #'unloading Metacello' }
ConfigurationOfVersionnerTestXMLWriter class >> unloadMetacello [

	Smalltalk at: #ConfigurationOfMetacello ifPresent: [:cls | cls unloadMetacello ]
]

{ #category : #baselines }
ConfigurationOfVersionnerTestXMLWriter >> baseline100: spec [ 
	<version: '1.0-baseline'>
	
	spec for: #common do: [
		spec
			blessing: #baseline;
			repository: 'http://www.squeaksource.com/XMLWriter'.

		spec 
			package: 'Collections-CharacterMap' with: [
				spec repository: 'http://www.squeaksource.com/CharacterMap'];
			package: 'XML-Writer' with: [spec requires: 'Collections-CharacterMap'].

		spec
			group: 'default' with: #('Core');
			group: 'Core' with: #('XML-Writer');
			group: 'Tests' with: #()].
]

{ #category : #baselines }
ConfigurationOfVersionnerTestXMLWriter >> baseline101: spec [ 
	<version: '1.0.1-baseline'>
	
	spec for: #common do: [
		spec
			blessing: #baseline;
			repository: 'http://www.squeaksource.com/XMLWriter'.

		spec
			package: 'Collections-OrderPreservingDictionary' with: [
				spec repository: 'http://www.squeaksource.com/OrderPreservingDict'];
			package: 'Collections-CharacterMap' with: [
				spec repository: 'http://www.squeaksource.com/CharacterMap'];
			package: 'XML-Writer' with: [
				spec requires: #('Collections-OrderPreservingDictionary' 'Collections-CharacterMap')].

		spec
			group: 'default' with: #('Core');
			group: 'Core' with: #('XML-Writer')].
]

{ #category : #baselines }
ConfigurationOfVersionnerTestXMLWriter >> baseline104: spec [ 
	<version: '1.0.4-baseline'>
	
	spec for: #common do: [
		spec
			blessing: #baseline;
			repository: 'http://www.squeaksource.com/XMLWriter'.

		spec
			package: 'Collections-OrderPreservingDictionary' with: [
				spec repository: 'http://www.squeaksource.com/OrderPreservingDict'];
			package: 'XML-Writer' with: [
				spec requires: #('Collections-OrderPreservingDictionary')].

		spec
			group: 'default' with: #('Core');
			group: 'Core' with: #('XML-Writer')].
		
	spec for: #gemstone do: [
		spec
			package: 'Collections-CharacterSet'.
		spec 
			package: 'XML-Writer' with:[spec requires: #('Collections-CharacterSet')]]
]

{ #category : #baselines }
ConfigurationOfVersionnerTestXMLWriter >> baseline105: spec [
	<version: '1.0.5-baseline'>
	spec for: #common do: [ 
		spec
			blessing: #baseline;
			repository: 'http://www.squeaksource.com/XMLWriter'.
		spec
			package: 'Collections-OrderPreservingDictionary' with: [ 
				spec repository: 'http://www.squeaksource.com/OrderPreservingDict' ];
			package: 'Collections-Support';
			package: 'XML-Writer' with: [ 
				spec requires: #('Collections-Support' 'Collections-OrderPreservingDictionary') ].
		spec
			group: 'default' with: #('Core');
			group: 'Core' with: #('XML-Writer');
			group: 'Tests' with: #() ].
		
	spec for: #squeakCommon do: [ 
		spec package: 'Collections-Support' with: [ 
			spec repository: 'http://www.squeaksource.com/Pharo' ] ].
	
	spec for: #gemstone do: [ 
		spec
			package: 'Collections-Support' with: [ 
				spec
					file: 'Collections-Support.g';
					repository: 'http://seaside.gemstone.com/ss/XMLSupport' ];
			package: 'Collections-Tests-Support' with: [ 
				spec
					file: 'Collections-Tests-Support.g';
					requires: 'Collections-Support';
					repository: 'http://seaside.gemstone.com/ss/XMLSupport' ].
		spec
			group: 'Tests' with: #('Collections-Tests-Support') ]
]

{ #category : #baselines }
ConfigurationOfVersionnerTestXMLWriter >> baseline200: spec [
	<version: '2.0.0-baseline'>

	spec for: #common do: [ 
		spec
			blessing: #baseline;
			description: 'Migrated to SmalltalkHub and taking into account new configurations of subcomponents';
			repository: 'http://smalltalkhub.com/mc/Pharo/XMLWriter/main'.
				
		"Does not work without a className: specification
		
		spec project: 'OrderPreservingDictionary' with: [
			spec 
				versionString: #stable; 
				loads: #('Core'); 
				repository: 'http://smalltalkhub.com/mc/Pharo/OrderPreservingDictionary/main'].
		spec package: 'XML-Writer' with: [ spec requires: #('OrderPreservingDictionary') ]."
			
		spec project: 'OrderPreservingDictionary' with: [
			spec 
				className: 'ConfigurationOfOrderPreservingDictionary';
				versionString: #stable; 
				loads: #('Core'); 
				repository: 'http://smalltalkhub.com/mc/Pharo/OrderPreservingDictionary/main'].	
				
		spec package: 'XML-Writer' with: [spec requires: 'OrderPreservingDictionary']. 
			
		spec
			group: 'default' with: #('Core');
			group: 'Core' with: #('XML-Writer');
			group: 'Tests' with: #() ].
	
	
	
	"I removed it because it loads an old version of the pckage and this is a nonsense"	
	spec for: #squeakCommon do: [ 
		spec package: 'Collections-Support' with: [ 
			spec repository: 'http://ss3.gemstone.com/ss/Pharo20' ] ].
	
	
	
	
	spec for: #gemstone do: [ 
		spec
			package: 'Collections-Support' with: [ 
				spec
					file: 'Collections-Support.g';
					repository: 'http://seaside.gemstone.com/ss/XMLSupport' ];
			package: 'Collections-Tests-Support' with: [ 
				spec
					file: 'Collections-Tests-Support.g';
					requires: 'Collections-Support';
					repository: 'http://seaside.gemstone.com/ss/XMLSupport' ].
		spec
			group: 'Tests' with: #('Collections-Tests-Support') ]
]

{ #category : #baselines }
ConfigurationOfVersionnerTestXMLWriter >> baseline210: spec [
	<version: '2.1.0-baseline'>

	spec for: #common do: [ 
		spec
			blessing: #baseline;
			description: 'Adding a test package';
			repository: 'http://smalltalkhub.com/mc/Pharo/XMLWriter/main'.
				
		spec project: 'OrderPreservingDictionary' with: [
			spec 
				className: 'ConfigurationOfOrderPreservingDictionary';
				versionString: #stable; 
				loads: #('Core'); 
				repository: 'http://smalltalkhub.com/mc/Pharo/OrderPreservingDictionary/main'].	
				
		spec package: 'XML-Writer-Core' with: [spec requires: 'OrderPreservingDictionary']. 
		spec package: 'XML-Writer-Tests' with: [spec requires: 'XML-Writer-Core']. 
		
		spec
			group: 'default' with: #('Core');
			group: 'Core' with: #('XML-Writer-Core');
			group: 'Tests' with: #('XML-Writer-Tests')].
	
	
	"Can only work for Pharo1.4 and 2.0.
	To be changed when pharo will be managed on SmalltalkHub"
		
	spec for: #squeakCommon do: [ 
		spec package: 'Collections-Support' with: [ 
			spec repository: 'http://ss3.gemstone.com/ss/Pharo20' ] ].
	
	
	"I did not touch this part because Ido not know it. On Pharo XMLWriter has its own repository"
	spec for: #gemstone do: [ 
		spec
			package: 'Collections-Support' with: [ 
				spec
					file: 'Collections-Support.g';
					repository: 'http://seaside.gemstone.com/ss/XMLSupport' ];
			package: 'Collections-Tests-Support' with: [ 
				spec
					file: 'Collections-Tests-Support.g';
					requires: 'Collections-Support';
					repository: 'http://seaside.gemstone.com/ss/XMLSupport' ].
		spec
			group: 'Tests' with: #('Collections-Tests-Support') ]
]

{ #category : #symbolic }
ConfigurationOfVersionnerTestXMLWriter >> development: spec [
	<symbolicVersion: #'development'>
	
	spec for: #'pharo' version: '2.1.0'.

]

{ #category : #accessing }
ConfigurationOfVersionnerTestXMLWriter >> project [

	^ project ifNil: [ | constructor |
		"Bootstrap Metacello if it is not already loaded"
		self class ensureMetacello.
		"Construct Metacello project"
		constructor := (Smalltalk at: #MetacelloVersionConstructor) on: self.
		project := constructor project.
		project loadType: #linear. "change to #atomic if desired"
		project ]
]

{ #category : #symbolic }
ConfigurationOfVersionnerTestXMLWriter >> stable: spec [
	<symbolicVersion: #'stable'>
	
	spec for: #'pharo' version: '2.1.0'.

]

{ #category : #versions }
ConfigurationOfVersionnerTestXMLWriter >> version100: spec [ 
	<version: '1.0' imports: #('1.0-baseline') >
	
	spec for: #common do: [
		spec blessing: #release.

		spec
			package: 'Collections-CharacterMap' with: 'Collections-CharacterMap-JAAyer.1';
			package: 'XML-Writer' with: 'XML-Writer-JAAyer.1'].
]

{ #category : #versions }
ConfigurationOfVersionnerTestXMLWriter >> version101: spec [ 
	<version: '1.0.1' imports: #('1.0.1-baseline') >
	
	spec for: #common do: [
		spec blessing: #release.

		spec
			package: 'Collections-OrderPreservingDictionary'
			with: 'Collections-OrderPreservingDictionary-JAAyer.1';
			package: 'Collections-CharacterMap' with: 'Collections-CharacterMap-JAAyer.1';
			package: 'XML-Writer' with: 'XML-Writer-JAAyer.2'].
]

{ #category : #versions }
ConfigurationOfVersionnerTestXMLWriter >> version102: spec [ 
	<version: '1.0.2' imports: #('1.0.1-baseline') >
	
	spec for: #common do: [
		spec blessing: #release.

		spec
			package: 'Collections-OrderPreservingDictionary'
			with: 'Collections-OrderPreservingDictionary-JAAyer.1';
			package: 'Collections-CharacterMap' with: 'Collections-CharacterMap-JAAyer.1';
			package: 'XML-Writer' with: 'XML-Writer-JAAyer.3'].
]

{ #category : #versions }
ConfigurationOfVersionnerTestXMLWriter >> version103: spec [ 
	<version: '1.0.3' imports: #('1.0.1-baseline') >
	
	spec for: #common do: [
		spec blessing: #release.

		spec
			package: 'Collections-OrderPreservingDictionary'
			with: 'Collections-OrderPreservingDictionary-JAAyer.5';
			package: 'Collections-CharacterMap' with: 'Collections-CharacterMap-JAAyer.1';
			package: 'XML-Writer' with: 'XML-Writer-JAAyer.4'].
]

{ #category : #versions }
ConfigurationOfVersionnerTestXMLWriter >> version104: spec [ 
	<version: '1.0.4' imports: #('1.0.4-baseline') >
	
	spec for: #common do: [
		spec blessing: #release.

		spec
			package: 'Collections-OrderPreservingDictionary'
			with: 'Collections-OrderPreservingDictionary-JAAyer.6';
			package: 'XML-Writer' with: 'XML-Writer-JAAyer.5'].

	spec for: #gemstone do: [
		spec 
			package: 'Collections-CharacterSet' with: 'Collections-CharacterSet-JohanBrichau.1']
]

{ #category : #versions }
ConfigurationOfVersionnerTestXMLWriter >> version105: spec [
	<version: '1.0.5' imports: #('1.0.5-baseline')>
	spec
		for: #common
			do: [ 
					spec
						blessing: #release;
						package: 'Collections-OrderPreservingDictionary' with: 'Collections-OrderPreservingDictionary-JAAyer.6';
						package: 'XML-Writer' with: 'XML-Writer-JAAyer.5' ];
		for: #squeakCommon do: [ spec package: 'Collections-Support' with: 'Collections-Support-StephaneDucasse.35' ];
		for: #gemstone
			do: [ 
					spec
						package: 'Collections-Support' with: 'Collections-Support.g-OttoBehrens.45';
						package: 'Collections-Tests-Support' with: 'Collections-Tests-Support.g-OttoBehrens.3' ]
]

{ #category : #versions }
ConfigurationOfVersionnerTestXMLWriter >> version200: spec [
	<version: '2.0.0' imports: #('2.0.0-baseline')>
	spec
		for: #common
			do: [ spec
					blessing: #release;
					package: 'XML-Writer' with: 'XML-Writer-JAAyer.5' ];
		
		" I do not understand the following line because it means that we would load an older version than the one currently in Pharo. Clearly the best way to introduce bugs and losing hours trying to understand why.
		for: #squeakCommon do: [ spec package: 'Collections-Support' with: 'Collections-Support-StephaneDucasse.35' ];"
		for: #gemstone
			do: [ 
					spec
						package: 'Collections-Support' with: 'Collections-Support.g-OttoBehrens.45';
						package: 'Collections-Tests-Support' with: 'Collections-Tests-Support.g-OttoBehrens.3' ]
]

{ #category : #versions }
ConfigurationOfVersionnerTestXMLWriter >> version210: spec [
	<version: '2.1.0' imports: #('2.1.0-baseline')>
	spec
		for: #common
			do: [ spec
					blessing: #release;
					package: 'XML-Writer-Core' with: 'XML-Writer-Core-StephaneDucasse.1' ;
					package: 'XML-Writer-Tests' with: 'XML-Writer-Tests-StephaneDucasse.1' ];
		
		" I do not understand the following line because it means that we would load an older version than the one currently in Pharo. Clearly the best way to introduce bugs and losing hours trying to understand why.
		for: #squeakCommon do: [ spec package: 'Collections-Support' with: 'Collections-Support-StephaneDucasse.35' ];"
		for: #gemstone
			do: [ 
					spec
						package: 'Collections-Support' with: 'Collections-Support.g-OttoBehrens.45';
						package: 'Collections-Tests-Support' with: 'Collections-Tests-Support.g-OttoBehrens.3' ]
]

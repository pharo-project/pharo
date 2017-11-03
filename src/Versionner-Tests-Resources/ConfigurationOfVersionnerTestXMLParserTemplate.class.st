"
Simple Configuration class used to test Versionner behavior.
This class is a rename of ConfigurationOfXMLParser-StephaneDucasse.11.

"
Class {
	#name : #ConfigurationOfVersionnerTestXMLParserTemplate,
	#superclass : #Object,
	#instVars : [
		'project'
	],
	#classVars : [
		'LastVersionLoad'
	],
	#category : #Versionner-Tests-Resources
}

{ #category : #private }
ConfigurationOfVersionnerTestXMLParserTemplate class >> bootstrapPackage: aString from: aPath [ 
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
ConfigurationOfVersionnerTestXMLParserTemplate class >> ensureMetacello [
	"Bootstrap Gofer (if necessary), bootstrap ConfigurationOfMetacello (using old Gofer API), then load the latest 
	version of Metacello itself."

	Smalltalk at: #MetacelloProject ifAbsent: [
		Smalltalk at: #Gofer ifAbsent: [
			"Current version of Gofer from which to bootstrap - as of 1.0-beta.21"
			self bootstrapPackage: 'Gofer-Core-lr.115' from: 'http://seaside.gemstone.com/ss/metacello' ].
		Smalltalk at: #Gofer ifPresent: [:goferClass | | gofer |
			gofer := goferClass new
				url: 'http://seaside.gemstone.com/ss/metacello';
				yourself.
			[ gofer addPackage: 'ConfigurationOfMetacello' ] on: Warning do: [:ex | ex resume ].
			gofer load ].
		"load 'default' group of Metacello"
		(Smalltalk at: #ConfigurationOfMetacello) perform: #load ]
]

{ #category : #testing }
ConfigurationOfVersionnerTestXMLParserTemplate class >> isMetacelloConfig [
	"Answer true and the Metacello tools will operate on you"
	
	^true
]

{ #category : #'metacello tool support' }
ConfigurationOfVersionnerTestXMLParserTemplate class >> lastMetacelloVersionLoad [
	"Answer the last version loaded and the list of packages loaded for that version."

	LastVersionLoad == nil ifTrue: [ LastVersionLoad := nil -> 'default' ].
	^LastVersionLoad
]

{ #category : #loading }
ConfigurationOfVersionnerTestXMLParserTemplate class >> load [
	"self load"

	^self project latestVersion load
]

{ #category : #loading }
ConfigurationOfVersionnerTestXMLParserTemplate class >> loadDefault [
	"self loadDefault"

	^ self loadMostRecentCoreWithTests
]

{ #category : #loading }
ConfigurationOfVersionnerTestXMLParserTemplate class >> loadMostRecentCoreWithTests [
	^self project lastVersion load: #('Core' 'Tests')
]

{ #category : #'metacello tool support' }
ConfigurationOfVersionnerTestXMLParserTemplate class >> metacelloVersion: versionString loads: anArrayOrString [
	"Stash the last version loaded and the list of packages loaded for that version. The list
	 of packages will be used by the tools when doing 'Load Package Version'"

	LastVersionLoad := versionString -> anArrayOrString
]

{ #category : #accessing }
ConfigurationOfVersionnerTestXMLParserTemplate class >> project [

	^self new project
]

{ #category : #'unloading Metacello' }
ConfigurationOfVersionnerTestXMLParserTemplate class >> unloadMetacello [

	Smalltalk at: #ConfigurationOfMetacello ifPresent: [:cls | cls unloadMetacello ]
]

{ #category : #baselines }
ConfigurationOfVersionnerTestXMLParserTemplate >> baseline10: spec [
	<version: '1.0-baseline'>
	spec
		for: #common
		do: [ 
			spec
				blessing: #baseline;
				description: 'Copy of ConfigurationOfXMLParser used with Versionner tests.';
				author: 'ChristopheDemarey';
				timestamp: '27/03/2013';
				repository: 'http://www.smalltalkhub.com/mc/PharoExtras/XMLParser/main'.

			spec project: 'XMLWriter'
					with: [ spec 
								className: 'ConfigurationOfVersionnerTestXMLWriter';
								versionString: #stable;
								file: 'Versionner-Tests-Resources';
								repository: 'http://www.smalltalkhub.com/mc/demarey/Versionner/main'.].

			spec project: 'BitmapCharacterSet'
					with: [ spec 
								className: 'ConfigurationOfVersionnerTestBitmapCharacterSet';
								versionString: #stable;
								file: 'Versionner-Tests-Resources';
								repository: 'http://www.smalltalkhub.com/mc/demarey/Versionner/main'.].
			
			spec 
				package: 'VersionnerTestXML-Parser' 
				with: [spec 
						requires: #('BitmapCharacterSet' 'XMLWriter')].	
			spec package: 'VersionnerTestXML-Tests-Parser'
					with: [ spec requires: 'VersionnerTestXML-Parser' ].
			
			spec
				group: 'default' with: #('Core' 'Tests');
				"default by default is equivalent to loading 'ALL' the packages still we added explicitly Tests to be loaded"
				group: 'Core' with: #('VersionnerTestXML-Parser');
				group: 'Tests' with: #('VersionnerTestXML-Tests-Parser') ].

]

{ #category : #baselines }
ConfigurationOfVersionnerTestXMLParserTemplate >> baseline20: spec [
	<version: '2.0-baseline'>
	spec
		for: #common
		do: [ 
			spec
				blessing: #baseline;
				description: 'New baseline to test use of baselines in numbered versions';
				author: 'ChristopheDemarey';
				timestamp: '25/02/2014';
				repository: 'http://www.smalltalkhub.com/mc/PharoExtras/XMLParser/main'.

			spec project: 'XMLWriter'
					with: [ spec 
								className: 'ConfigurationOfVersionnerTestXMLWriter';
								versionString: #stable;
								file: 'Versionner-Tests-Resources';
								repository: 'http://www.smalltalkhub.com/mc/demarey/Versionner/main'.].

			spec project: 'BitmapCharacterSet'
					with: [ spec 
								className: 'ConfigurationOfVersionnerTestBitmapCharacterSet';
								versionString: #stable;
								file: 'Versionner-Tests-Resources';
								repository: 'http://www.smalltalkhub.com/mc/demarey/Versionner/main'.].
			
			spec 
				package: 'VersionnerTestXML-Parser' 
				with: [spec 
						requires: #('BitmapCharacterSet' 'XMLWriter')].	
			spec package: 'VersionnerTestXML-Tests-Parser'
					with: [ spec requires: 'VersionnerTestXML-Parser' ].
			
			spec
				group: 'default' with: #('Core' 'Tests');
				"default by default is equivalent to loading 'ALL' the packages still we added explicitly Tests to be loaded"
				group: 'Core' with: #('VersionnerTestXML-Parser');
				group: 'Tests' with: #('VersionnerTestXML-Tests-Parser') ].

]

{ #category : #'symbolic versions' }
ConfigurationOfVersionnerTestXMLParserTemplate >> development: spec [
	<symbolicVersion: #'development'>

	spec for: #'common' version: '1.1'.
	
]

{ #category : #accessing }
ConfigurationOfVersionnerTestXMLParserTemplate >> project [

	^ project ifNil: [ | constructor |
		"Bootstrap Metacello if it is not already loaded"
		self class ensureMetacello.
		"Construct Metacello project"
		constructor := (Smalltalk at: #MetacelloVersionConstructor) on: self.
		project := constructor project.
		project loadType: #linear. "change to #atomic if desired"
		project ]
]

{ #category : #'symbolic versions' }
ConfigurationOfVersionnerTestXMLParserTemplate >> stable: spec [
	<symbolicVersion: #'stable'>

	spec for: #'common' version: '1.1'.
	
]

{ #category : #versions }
ConfigurationOfVersionnerTestXMLParserTemplate >> version100: spec [
	<version: '1.0' imports: #('1.0-baseline')>
	spec
		for: #common
		do: [ 
			spec blessing: #release.
			spec
				package: 'VersionnerTestXML-Parser' with: 'XML-Parser-NorbertHartl.141';
				package: 'VersionnerTestXML-Tests-Parser' with: 'XML-Tests-Parser-JAA.14' 
				].
	
	
]

{ #category : #versions }
ConfigurationOfVersionnerTestXMLParserTemplate >> version101: spec [
	<version: '1.1' imports: #('1.0-baseline')>
	spec
		for: #common
		do: [ 
			spec blessing: #release.
			spec
				package: 'VersionnerTestXML-Parser' with: 'VersionnerTestXML-Parser-StephaneDucasse.142';
				package: 'VersionnerTestXML-Tests-Parser' with: 'VersionnerTestXML-Tests-Parser-StephaneDucasse.15'. ].
	
	
]

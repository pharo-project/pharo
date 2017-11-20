"
Simple Configuration class used to test Versionner behavior.
This class is a rename of ConfigurationOfVersionnerTestBitmapCharacterSet-StephaneDucasse.2.


"
Class {
	#name : #ConfigurationOfVersionnerTestBitmapCharacterSet,
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
ConfigurationOfVersionnerTestBitmapCharacterSet class >> bootstrapPackage: aString from: aPath [ 
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
ConfigurationOfVersionnerTestBitmapCharacterSet class >> ensureMetacello [
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

{ #category : #'metacello tool support' }
ConfigurationOfVersionnerTestBitmapCharacterSet class >> isMetacelloConfig [
	"Answer true and the Metacello tools will operate on you"
	
	^true
]

{ #category : #'metacello tool support' }
ConfigurationOfVersionnerTestBitmapCharacterSet class >> lastMetacelloVersionLoad [
	"Answer the last version loaded and the list of packages loaded for that version."

	LastVersionLoad == nil ifTrue: [ LastVersionLoad := nil -> 'default' ].
	^LastVersionLoad
]

{ #category : #loading }
ConfigurationOfVersionnerTestBitmapCharacterSet class >> load [
	"self load"

	^self project latestVersion load
]

{ #category : #loading }
ConfigurationOfVersionnerTestBitmapCharacterSet class >> loadDefault [
	"self loadDefault"

	^ self loadMostRecentCoreWithTests
]

{ #category : #loading }
ConfigurationOfVersionnerTestBitmapCharacterSet class >> loadMostRecentCoreWithTests [
	^self project lastVersion load: #('Core' 'Tests')
]

{ #category : #'metacello tool support' }
ConfigurationOfVersionnerTestBitmapCharacterSet class >> metacelloVersion: versionString loads: anArrayOrString [
	"Stash the last version loaded and the list of packages loaded for that version. The list
	 of packages will be used by the tools when doing 'Load Package Version'"

	LastVersionLoad := versionString -> anArrayOrString
]

{ #category : #accessing }
ConfigurationOfVersionnerTestBitmapCharacterSet class >> project [

	^self new project
]

{ #category : #'unloading Metacello' }
ConfigurationOfVersionnerTestBitmapCharacterSet class >> unloadMetacello [

	Smalltalk at: #ConfigurationOfMetacello ifPresent: [:cls | cls unloadMetacello ]
]

{ #category : #baselines }
ConfigurationOfVersionnerTestBitmapCharacterSet >> baseline100: spec [ 
	<version: '1.0-baseline'>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec repository: 'http://smalltalkhub.com/mc/PharoExtras/BitmapCharacterSet/main'.	
		spec description: 'Imported from SqueakSource'.
		spec package: 'Collections-BitmapCharacterSet'.
		spec 
			package: 'Collections-Tests-BitmapCharacterSet' 
			with: [spec requires: 'Collections-BitmapCharacterSet'].
			
		spec
			group: 'default' with: #('Core');
			group: 'Core' with: #('Collections-BitmapCharacterSet');
			group: 'Tests' with: #('Collections-Tests-BitmapCharacterSet'). ].
]

{ #category : #'symbolic versions' }
ConfigurationOfVersionnerTestBitmapCharacterSet >> development: spec [
	<symbolicVersion: #'development'>

	spec for: #'common' version: '1.0'

]

{ #category : #accessing }
ConfigurationOfVersionnerTestBitmapCharacterSet >> project [

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
ConfigurationOfVersionnerTestBitmapCharacterSet >> stable: spec [
	<symbolicVersion: #'stable'>

	spec for: #'common' version: '1.0'.
	
]

{ #category : #versions }
ConfigurationOfVersionnerTestBitmapCharacterSet >> version100: spec [ 
	<version: '1.0' imports: #('1.0-baseline') >
	
	spec for: #common do: [
		spec blessing: #release.
		spec description: 'Latest versions from Squeaksource'.
		
		spec package: 'Collections-BitmapCharacterSet' with: ' Collections-BitmapCharacterSet-JAA.5'.
		spec package: 'Collections-Tests-BitmapCharacterSet' with: 'Collections-Tests-BitmapCharacterSet-JAA.5'
		]
	


]

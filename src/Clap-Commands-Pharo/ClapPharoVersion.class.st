"
I report release and version information about the image and VM, in various formats.
"
Class {
	#name : 'ClapPharoVersion',
	#superclass : 'ClapPharoApplication',
	#category : 'Clap-Commands-Pharo',
	#package : 'Clap-Commands-Pharo'
}

{ #category : 'command line' }
ClapPharoVersion class >> version [
	<commandline>
	
	^ (ClapCommandSpec id: #version)
		description: 'Displays version information, in various formats';
		commandClass: self;
		addHelp;
		addFlag: #full description: 'Full image version (default format)';
		addFlag: #release description: 'Major.minor alpha/stable';
		addFlag: #numeric description: '5-digit sequential integration number';
		addFlag: #hash description: 'Integration commit hash';
		addFlag: #vm description: 'VM build and version';
		addFlag: #license description: 'Licensing and copyright';
		yourself
]

{ #category : 'execution' }
ClapPharoVersion >> execute [
	self showVersionsNamed: self formats
]

{ #category : 'accessing' }
ClapPharoVersion >> formatSelectorFor: formatName [
	^ formatName asSymbol , #VersionString
]

{ #category : 'accessing' }
ClapPharoVersion >> formatStringFor: formatName [
	^ self perform: (self formatSelectorFor: formatName)
]

{ #category : 'accessing' }
ClapPharoVersion >> formats [
	^ self flags ifEmpty: [ #(full) ]
]

{ #category : 'version strings' }
ClapPharoVersion >> fullVersionString [
	^ SystemVersion current imageVersionString
]

{ #category : 'version strings' }
ClapPharoVersion >> hashVersionString [
	^ SystemVersion current commitHash
]

{ #category : 'version strings' }
ClapPharoVersion >> licenseVersionString [
	^ Smalltalk licenseString
]

{ #category : 'version strings' }
ClapPharoVersion >> numericVersionString [
	^ SystemVersion current highestUpdate printString
]

{ #category : 'version strings' }
ClapPharoVersion >> releaseVersionString [
	^ SystemVersion current shortVersionString
]

{ #category : 'printing' }
ClapPharoVersion >> showVersionsNamed: formatNames [
	| out |
	out := self context stdout.
	formatNames do: [ :each | 
		out nextPutAll: (self formatStringFor: each); lf ].
	out flush
]

{ #category : 'version strings' }
ClapPharoVersion >> vmVersionString [
	^ Smalltalk vm version
]

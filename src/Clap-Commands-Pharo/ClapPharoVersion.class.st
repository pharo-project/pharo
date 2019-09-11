"
I report release and version information about the image and VM, in various formats.
"
Class {
	#name : #ClapPharoVersion,
	#superclass : #Object,
	#instVars : [
		'stream'
	],
	#category : #'Clap-Commands-Pharo'
}

{ #category : #'instance creation' }
ClapPharoVersion class >> showVersionsNamed: formatNames [
	self new
		showVersionsNamed: formatNames
]

{ #category : #'command line' }
ClapPharoVersion class >> version [
	<commandline>
	^ (ClapCommand withName: 'version')
		description: 'Displays version information, in various formats';
		add: ClapFlag forHelp;
		add: ((ClapFlag withName: 'full') description: 'Full image version (default format)');
		add: ((ClapFlag withName: 'release') description: 'Major.minor alpha/stable');
		add: ((ClapFlag withName: 'numeric') description: '5-digit sequential integration number');
		add: ((ClapFlag withName: 'hash') description: 'Integration commit hash');
		add: ((ClapFlag withName: 'vm') description: 'VM build and version');
		add: ((ClapFlag withName: 'license') description: 'Licensing and copyright');
		meaning: [ :args | | formats |
			args atName: 'help' ifFound: [ :help |
				help value; exitSuccess ].
			args validateAll.

			formats := args flags collect: [ :each | each specification canonicalName asSymbol ].
			self showVersionsNamed: (formats ifEmpty: [ #(full) ]) ]
]

{ #category : #accessing }
ClapPharoVersion >> formatSelectorFor: formatName [
	^ formatName asSymbol , #VersionString
]

{ #category : #accessing }
ClapPharoVersion >> formatStringFor: formatName [
	^ self perform: (self formatSelectorFor: formatName)
]

{ #category : #'version strings' }
ClapPharoVersion >> fullVersionString [
	^ SystemVersion current imageVersionString
]

{ #category : #'version strings' }
ClapPharoVersion >> hashVersionString [
	^ SystemVersion current commitHash
]

{ #category : #initialization }
ClapPharoVersion >> initialize [
	stream := VTermOutputDriver stdout
]

{ #category : #'version strings' }
ClapPharoVersion >> licenseVersionString [
	^ Smalltalk licenseString
]

{ #category : #'version strings' }
ClapPharoVersion >> numericVersionString [
	^ SystemVersion current highestUpdate printString
]

{ #category : #'version strings' }
ClapPharoVersion >> releaseVersionString [
	^ SystemVersion current shortVersionString
]

{ #category : #printing }
ClapPharoVersion >> showVersionNamed: formatName [
	stream
		nextPutAll: (self formatStringFor: formatName);
		lf
]

{ #category : #printing }
ClapPharoVersion >> showVersionsNamed: formatNames [
	formatNames do: [ :each | self showVersionNamed: each ].
	stream flush
]

{ #category : #'version strings' }
ClapPharoVersion >> vmVersionString [
	^ Smalltalk vm version
]

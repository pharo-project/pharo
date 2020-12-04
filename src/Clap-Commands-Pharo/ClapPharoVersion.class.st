"
I report release and version information about the image and VM, in various formats.
"
Class {
	#name : #ClapPharoVersion,
	#superclass : #ClapPharoApplication,
	#category : #'Clap-Commands-Pharo'
}

{ #category : #'command line' }
ClapPharoVersion class >> version [
	<commandline>
	^ (ClapCommand id: #version)
		description: 'Displays version information, in various formats';
		add: ClapFlag forHelp;
		add: ((ClapFlag id: #full) description: 'Full image version (default format)');
		add: ((ClapFlag id: #release) description: 'Major.minor alpha/stable');
		add: ((ClapFlag id: #numeric) description: '5-digit sequential integration number');
		add: ((ClapFlag id: #hash) description: 'Integration commit hash');
		add: ((ClapFlag id: #vm) description: 'VM build and version');
		add: ((ClapFlag id: #license) description: 'Licensing and copyright');
		meaning: [ :args |
			args at: #helpFlag ifPresent: [ :help |
				help value; exitSuccess ].
			args validateAll.

			(self with: args) execute ]
]

{ #category : #execution }
ClapPharoVersion >> execute [
	self showVersionsNamed: self formats
]

{ #category : #accessing }
ClapPharoVersion >> formatSelectorFor: formatName [
	^ formatName asSymbol , #VersionString
]

{ #category : #accessing }
ClapPharoVersion >> formatStringFor: formatName [
	^ self perform: (self formatSelectorFor: formatName)
]

{ #category : #accessing }
ClapPharoVersion >> formats [
	^ (arguments flags
		collect: [ :each | each specification canonicalName asSymbol ])
		ifEmpty: [ #(full) ]
]

{ #category : #'version strings' }
ClapPharoVersion >> fullVersionString [
	^ SystemVersion current imageVersionString
]

{ #category : #'version strings' }
ClapPharoVersion >> hashVersionString [
	^ SystemVersion current commitHash
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
ClapPharoVersion >> showVersionsNamed: formatNames [
	| out |
	out := self context stdout.
	formatNames do: [ :each | 
		out nextPutAll: (self formatStringFor: each); lf ].
	out flush
]

{ #category : #'version strings' }
ClapPharoVersion >> vmVersionString [
	^ Smalltalk vm version
]

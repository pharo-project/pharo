Class {
	#name : #ClapPharoVersion,
	#superclass : #Object,
	#instVars : [
		'stream',
		'formats'
	],
	#category : #'Clap-Commands-Pharo'
}

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
		meaning: [ :args |
			args atName: 'help' ifFound: [ :help |
				help value; exitSuccess ].
			args validateAll.

			(self with: args) execute ]
]

{ #category : #'instance creation' }
ClapPharoVersion class >> with: arguments [
	^ self new
		setArguments: arguments;
		yourself
]

{ #category : #execution }
ClapPharoVersion >> execute [
	self formats do: [ :each |
		stream
			nextPutAll: (self perform: each);
			lf ].
	stream flush
]

{ #category : #accessing }
ClapPharoVersion >> formats [
	^ formats ifEmpty: [ #(full) ]
]

{ #category : #formats }
ClapPharoVersion >> full [
	^ SystemVersion current imageVersionString
]

{ #category : #formats }
ClapPharoVersion >> hash [
	^ SystemVersion current commitHash
]

{ #category : #initialization }
ClapPharoVersion >> initialize [
	stream := VTermOutputDriver stdout
]

{ #category : #formats }
ClapPharoVersion >> license [
	^ Smalltalk licenseString
]

{ #category : #formats }
ClapPharoVersion >> numeric [
	^ SystemVersion current highestUpdate printString
]

{ #category : #formats }
ClapPharoVersion >> release [
	^ SystemVersion current shortVersionString
]

{ #category : #initialization }
ClapPharoVersion >> setArguments: arguments [
	formats := arguments flags collect: [ :each |
		each specification canonicalName asSymbol ]
]

{ #category : #formats }
ClapPharoVersion >> vm [
	^ Smalltalk vm version
]

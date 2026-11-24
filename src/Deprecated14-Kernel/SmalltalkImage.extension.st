Extension { #name : 'SmalltalkImage' }

{ #category : '*Deprecated14-Kernel' }
SmalltalkImage >> changesFile [
	"Answer the file containing the current changes."

	self
		deprecated: 'Ask for the FileLocator instead of Smalltalk'
		in: #Pharo14
		transformWith: '`@rcv changesFile' -> 'FileLocator changes`'.
	^ FileLocator changes
]

{ #category : '*Deprecated14-Kernel' }
SmalltalkImage >> getSystemAttribute: aNumber [
	self
		deprecated: 'Use Smalltalk vm getSystemAttribute: instead'
		in: #Pharo14.
	^ self vm getSystemAttribute: aNumber
]

{ #category : '*Deprecated14-Kernel' }
SmalltalkImage >> isHeadless [

	self
		deprecated: 'This method will always answer true now. It should either be replaced by a usage of #isInteractive or be removed'
		in: #Pharo14.
	^ true
]

{ #category : '*Deprecated14-Kernel' }
SmalltalkImage >> isInteractiveGraphic [

	self
		deprecated: 'Use #isInteractive instead.'
		in: #Pharo14
		transformWith: '`@rcv isInteractiveGraphic' -> '`@rcv isInteractive'.
	^ self isInteractive
]

{ #category : '*Deprecated14-Kernel' }
SmalltalkImage >> sourceFileVersionString [

	self
		deprecated: 'This got delegated to the PharoFilesOpener.'
		in: #Pharo14
		transformWith: '`@rcv sourceFileVersionString' -> 'PharoFilesOpener sourceFileVersionString'.

	^ PharoFilesOpener sourceFileVersionString
]

{ #category : '*Deprecated14-Kernel' }
SmalltalkImage >> sourceFileVersionString: aString [

	self
		deprecated: 'This got delegated to the PharoFilesOpener.'
		in: #Pharo14
		transformWith: '`@rcv sourceFileVersionString: `@arg' -> 'PharoFilesOpener sourceFileVersionString: `@arg'.
	PharoFilesOpener sourceFileVersionString: aString
]

{ #category : '*Deprecated14-Kernel' }
SmalltalkImage >> sourcesFile [

	self
		deprecated: 'This got delegated to the PharoFilesOpener.'
		in: #Pharo14
		transformWith: '`@rcv sourceFile' -> 'PharoFilesOpener sourcesFile'.

	^ PharoFilesOpener sourcesFile
]

{ #category : '*Deprecated14-Kernel' }
SmalltalkImage >> sourcesName [

	self
		deprecated: 'This got delegated to the PharoFilesOpener.'
		in: #Pharo14
		transformWith: '`@rcv sourceName' -> 'PharoFilesOpener sourcesName'.

	^ PharoFilesOpener sourcesName
]

{ #category : '*Deprecated14-Kernel' }
SmalltalkImage >> sourcesSuffix [

	self
		deprecated: 'This got delegated to the PharoFilesOpener.'
		in: #Pharo14
		transformWith: '`@rcv sourcesSuffix' -> 'PharoFilesOpener sourcesSuffix'.
	^ PharoFilesOpener sourcesSuffix
]

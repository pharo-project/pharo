help
defaultTopic: pageSelector for: aConfigurationClass
	| topic stream helpTopicClass |
	(helpTopicClass := self helpTopicClass) ifNil: [^self error: 'Help system not installed'].
	stream := WriteStream on: String new.
	pageSelector == #helpInstallation
		ifTrue: [ 
			stream
				nextPutAll: 'To install this configuration, execute the following expression in a workspace:'; cr; tab.
			(aConfigurationClass project hasVersion: #stable)
				ifTrue: [ stream nextPutAll: '(', aConfigurationClass name asString, ' project version: #stable) load.' ]
				ifFalse: [ 
					(aConfigurationClass respondsTo: #load)
						ifTrue: [ stream nextPutAll: aConfigurationClass name asString , ' load.' ]
						ifFalse: [ stream nextPutAll: aConfigurationClass name asString, ' project latestVersion load.' ]].
			^ helpTopicClass perform: #title:contents: withArguments: { 'Installing'.  stream contents} ].
	pageSelector == #helpIntro
		ifTrue: [ 
			stream
				nextPutAll: 'I am a Metacello configuration for the ', (MetacelloToolBox baseNameOfConfiguration: aConfigurationClass), ' project.'.
			^ helpTopicClass perform: #title:contents: withArguments: { 'Intro' . stream contents} ].
	pageSelector == #helpGettingStarted
		ifTrue: [ 
			stream
				nextPutAll: 'I guess you''re on your own:).'.
			^ helpTopicClass perform: #title:contents: withArguments: { 'Getting Started' .stream contents} ].

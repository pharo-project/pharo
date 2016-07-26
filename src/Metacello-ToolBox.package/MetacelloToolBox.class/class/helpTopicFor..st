help
helpTopicFor: aConfigurationClass
	| topic pages page helpTopicClass |
	(helpTopicClass := self helpTopicClass) ifNil: [^self error: 'Help system not installed'].
	topic := helpTopicClass named: (self baseNameOfConfiguration: aConfigurationClass).
	pages := (aConfigurationClass respondsTo: #helpPages)
		ifTrue: [ aConfigurationClass perform: #helpPages ]
		ifFalse: [ #(#helpIntro #helpInstallation #helpGettingStarted) ].
	pages
		do: [ :pageSelectorOrClass | 
			page := (Smalltalk hasClassNamed: pageSelectorOrClass asString)
				ifTrue: [ (Smalltalk classNamed: pageSelectorOrClass asString) perform: #asHelpTopic ]
				ifFalse: [ 
					[ aConfigurationClass perform: pageSelectorOrClass ]
						on: MessageNotUnderstood
						do: [ :ex | self defaultTopic: pageSelectorOrClass for: aConfigurationClass ] ].
			topic perform: #addSubtopic: with: page ].
	^ topic
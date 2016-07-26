testing
interestedInConfigurationInfo: configInfo
	| projectClass |
	(projectClass := self spec projectClass) == nil
		ifTrue: [ ^ false ].
	^(self configurationRoot configurationInfoFor: projectClass) name = configInfo name

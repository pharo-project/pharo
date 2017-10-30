accessing
baselineOrConfigurationNameSuffix
	self isBaselineName
		ifTrue: [ ^ self baselineOrConfigurationName copyFrom: 'BaselineOf' size + 1 to: self baselineOrConfigurationName size ].
		
	self isConfigurationName
		ifTrue: [ ^ self baselineOrConfigurationName copyFrom: 'ConfigurationOf' size + 1 to: self baselineOrConfigurationName size ]
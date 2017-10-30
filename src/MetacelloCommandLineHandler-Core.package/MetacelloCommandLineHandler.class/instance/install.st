activation
install
	| metacello |
	metacello := Metacello new.
	self isBaselineName
		ifTrue: [ metacello baseline: self baselineOrConfigurationNameSuffix ].
	self isConfigurationName
		ifTrue: [ metacello configuration: self baselineOrConfigurationNameSuffix ].
	metacello repository: self repositoryUrl.
	self hasVersion
		ifTrue: [ metacello version: self version ].
	self hasGroups
		ifTrue: [ metacello load: self groups ]
		ifFalse: [ metacello load ]
accessing
configurationName
	^fileName ifNotNil: [(fileName findTokens: '/\:') last copyUpToLast: $.]
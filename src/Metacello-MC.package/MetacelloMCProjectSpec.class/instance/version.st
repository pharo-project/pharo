querying
version
	"Empty version string means use latestVersion or #bleedingEdge"

	self projectClass == nil
		ifTrue: [ ^ nil ].
	^ self versionString == nil
		ifTrue: [ 
			| vrsn |
			"Eventually it will become an error to not specify a project reference version as default: #stable is the preferred default"
			"self deprecated: 'Must specify a project reference version.'."
			self flag: 'deprecate after version 1.0'.
			(vrsn := self projectClassProject latestVersion) == nil
				ifTrue: [ self projectClassProject version: #bleedingEdge ]
				ifFalse: [ vrsn ] ]
		ifFalse: [ self projectClassProject version: self versionString ]
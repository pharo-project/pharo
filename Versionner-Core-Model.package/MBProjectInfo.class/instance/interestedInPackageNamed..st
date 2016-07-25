testing
interestedInPackageNamed: aString
	| projectClass version info |
	(projectClass := self spec projectClass) == nil
		ifTrue: [ ^ false ].
	(aString beginsWith: projectClass name asString)
		ifTrue: [ ^ true ].
	(info := self configurationRoot configurationInfoFor: projectClass) == nil
		ifTrue: [ ^ false ].
	^ info interestedInPackageNamed: aString
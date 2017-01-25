accessing
allAncestors
	^ self versionInfo 
		ifNotNil: [ :versionInfo | versionInfo allAncestors ]
		ifNil: [ #() ]
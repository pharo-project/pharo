accessing
nextAncestors
	^ self versionInfo 
		ifNotNil: [ :versionInfo | versionInfo ancestors ]
		ifNil: [ #() ]
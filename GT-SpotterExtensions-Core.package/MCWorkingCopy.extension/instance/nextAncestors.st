*GT-SpotterExtensions-Core-private
nextAncestors
	^ self versionInfo 
		ifNotNil: [ :versionInfo | versionInfo ancestors ]
		ifNil: [ #() ]
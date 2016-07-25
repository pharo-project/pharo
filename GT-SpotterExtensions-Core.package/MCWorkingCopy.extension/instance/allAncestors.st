*GT-SpotterExtensions-Core-private
allAncestors
	^ self versionInfo 
		ifNotNil: [ :versionInfo | versionInfo allAncestors ]
		ifNil: [ #() ]
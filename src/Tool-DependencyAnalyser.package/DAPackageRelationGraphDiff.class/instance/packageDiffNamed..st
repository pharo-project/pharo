accessing
packageDiffNamed: aName
	^ self packagesDiff detect: [ :item | item content packageName = aName ]
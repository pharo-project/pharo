class tags
classTagNamed: aSymbol ifPresent: aBlock
	^ classTags 
		detect: [ :each | each name = aSymbol ] 
		ifFound: [ :tag | aBlock cull: tag ] 
		ifNone: [ nil ]
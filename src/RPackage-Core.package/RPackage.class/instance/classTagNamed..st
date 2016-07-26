class tags
classTagNamed: aSymbol

	^ classTags detect: [ :each | each name = aSymbol ]
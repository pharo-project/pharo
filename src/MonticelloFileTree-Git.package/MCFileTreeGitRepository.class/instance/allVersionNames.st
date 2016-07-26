accessing
allVersionNames
	^ self readableFileNames collect: [ :ea | ea name ]
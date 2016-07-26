internal
streamDo: aBlock

	self contentsWrapped: (Text streamContents: aBlock).
	self extent: self minimumExtent.
	self height: (paragraph extent y + (self borderWidth * 2) + (margins ifNil: [0] ifNotNil: [margins top + margins bottom]) + 2).

	self vanishDelay: ((((self contents size /50)seconds)+1 seconds) max: self defaultVanishDelay).
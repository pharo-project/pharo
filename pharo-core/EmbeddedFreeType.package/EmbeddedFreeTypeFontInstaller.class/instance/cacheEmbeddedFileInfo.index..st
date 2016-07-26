accessing
cacheEmbeddedFileInfo: aFreeTypeEmbeddedFileInfo index: i

	(embeddedFileInfoCache at:{aFreeTypeEmbeddedFileInfo fileSize. i} ifAbsentPut:[ Set new ])
		add: aFreeTypeEmbeddedFileInfo
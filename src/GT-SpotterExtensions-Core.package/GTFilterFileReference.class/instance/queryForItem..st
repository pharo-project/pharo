accessing
queryForItem: each
	self flag: 'this is an ugly hack for #queryForFileReference. The iterator will manipulate the query being used for each item'.
	^ self context cacheAt: #queryForFileReference ifAbsent: [ query ] " performance optimization "
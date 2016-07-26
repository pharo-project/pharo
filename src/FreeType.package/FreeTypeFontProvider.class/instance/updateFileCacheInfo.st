loading and updating
updateFileCacheInfo.
	fileInfoCache values 
		do:[ :col |
			col copy do:[:each | | dir |
				dir := each absolutePath asFileReference.
				dir exists ifFalse: [ col remove: each ]]]
		displayingProgress: [ 'Updating cached file info' ].
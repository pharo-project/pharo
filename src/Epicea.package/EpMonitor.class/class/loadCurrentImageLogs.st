accessing
loadCurrentImageLogs

	^self loadAllLogs select: [ :each | each isBelongsToCurrentImage  ]
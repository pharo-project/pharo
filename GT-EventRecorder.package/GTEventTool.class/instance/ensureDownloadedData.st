api
ensureDownloadedData
	self defaultDownloadFilename exists ifFalse: [ self download ]
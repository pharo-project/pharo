ci
loadLatestUpdateListSilently
	"fetch the latest version of the update.list on the server"
	
	| result listToUse lastUpdate |
	(result := ZnEasy get: UpdateStreamer new updateFolderURL, self updatesListFileName) isSuccess
		ifFalse: [ self error: 'Cannot access update server' ].
	listToUse := result contents.			
	listToUse linesDo: [:each | lastUpdate := each].
	(lastUpdate beginsWith: self getLatestUpdateNumber asString)
		 ifTrue: ["Write this file as our current updates.list"
			self updatesListFileName asFileReference delete;
				writeStreamDo:  [:stream | stream nextPutAll: listToUse]]
		ifFalse: [ 
			self inform: 'Image update version did not match automatically fetched  update.list!  
			Proceed after downloading using the script ./getUpdatesList instead' ].
	
private
loadLatestUpdateList
	"fetch the latest version of the update.list on the server"
	
	| result listToUse lastUpdate |
	result := ZnClient new
		systemPolicy;
		beOneShot;
		setAcceptEncodingGzip;
		accept: ZnMimeType text;
		url: (UpdateStreamer new updateFolderURL, self updatesListFileName);
		ifFail: [ :exception | self error: 'Cannot access update server: ' , exception printString ];
		get.
	(listToUse := result lines) isEmpty 
		ifTrue: [ self error: 'Update list is empty' ].			
	lastUpdate := listToUse last.
	(lastUpdate beginsWith: self getLatestUpdateNumber asString)
		 ifTrue: ["Write this file as our current updates.list"
			self updatesListFileName asFileReference
				delete;
				writeStreamDo: [:stream | stream nextPutAll: result ] ]
		ifFalse: [ 
			self inform: 'Image update version did not match automatically fetched  update.list!  
			Proceed after downloading using the script ./getUpdatesList instead' ].
	
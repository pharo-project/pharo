load and saving
load
	^ self preferences readStreamDo: [ :aReadStream |
			STON fromStream: aReadStream ]
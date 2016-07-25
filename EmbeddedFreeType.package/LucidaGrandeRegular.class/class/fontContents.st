accessing
fontContents
	^ self fontReference 
		readStreamDo: [ :stream | stream binary; contents ]
	
	
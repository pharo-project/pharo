extraction
extract

	^ self extractMetadata
		at: #payload put: self extractPayload;
		yourself
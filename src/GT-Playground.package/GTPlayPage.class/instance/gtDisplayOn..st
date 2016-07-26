ui
gtDisplayOn: stream
	stream nextPutAll: (self contentString copyReplaceAll: String cr with: String space)
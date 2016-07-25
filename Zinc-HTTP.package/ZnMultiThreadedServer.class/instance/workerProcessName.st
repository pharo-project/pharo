private
workerProcessName
	^ String streamContents: [ :stream |
		stream nextPutAll: self class name; nextPutAll: ' HTTP worker' ]
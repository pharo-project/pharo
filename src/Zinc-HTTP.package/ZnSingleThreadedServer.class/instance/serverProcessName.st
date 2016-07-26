private
serverProcessName
	^ String streamContents: [ :stream |
		stream nextPutAll: self class name; nextPutAll: ' HTTP port '; print: self port ]
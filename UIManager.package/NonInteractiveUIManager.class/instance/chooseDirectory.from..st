ui requests
chooseDirectory: label from: dir
	"Let the user choose a directory"
	^ self nonInteractiveWarning: (String streamContents: [ :s|
			s 
				nextPutAll: 'Interactive Directory Choice Request: ';
				nextPutAll: '[ '; nextPutAll: label asString; nextPutAll: ' ] from '; print: dir
		])
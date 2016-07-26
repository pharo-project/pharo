ui requests
edit: aText label: labelString accept: aBlockOrNil
	"Open an editor on the given string/text"
	^self nonInteractiveWarning: (String streamContents: [ :s |
		s  nextPutAll: 'Editing Text: '.
		labelString ifNotNil: [ s nextPutAll: '[ '; nextPutAll: labelString; nextPutAll: ' ] '].
		s cr; nextPutAll: aText])
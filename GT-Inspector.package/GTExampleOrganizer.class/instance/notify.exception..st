private-notifying
notify: aString exception: anException
	UIManager default inform: (String streamContents: [ :stream | stream 
		nextPutAll: aString;
		nextPutAll: ': ';
		nextPutAll: anException messageText ])
private-notifying
notifyForNumberOfArguments: anExample
	anExample ignoreNotification ifTrue: [ ^ self ].
	UIManager default inform: (String streamContents: [ :stream | stream 
		nextPutAll: 'Invalid number of arguments in' ;
		cr;
		nextPutAll: anExample method methodClass name asString;
		nextPutAll: ' >> #';
		nextPutAll: anExample method selector asString ])
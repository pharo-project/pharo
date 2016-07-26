private-notifying
notifyForDependencyUsing: aCompiledMethod example: anExample
	anExample ignoreNotification ifTrue: [ ^ self ].
	UIManager default inform: (String streamContents: [ :stream | stream 
		nextPutAll: 'Obsolete example dependency in' ;
		cr;
		nextPutAll: anExample method methodClass name asString;
		nextPutAll: ' >> #';
		nextPutAll: anExample method selector asString ])
private-notifying
notifyForMissingDependency: aDependency example: anExample
	anExample ignoreNotification ifTrue: [ ^ self ].
	UIManager default inform: (String streamContents: [ :stream | stream 
		nextPutAll: 'Missing example dependency' ;
		cr;
		nextPutAll: aDependency owner provider class name asString;
		nextPutAll: ' >> #';
		nextPutAll: aDependency selector asString ])
settings
settingDescription
	
	^ String streamContents: [ :stream |
		stream nextPutAll: 'The selected strategy defines how to handle exceptions that occur during a renraku validation of an entity.'.
		stream cr.
		
		self allSubclassesDo: [ :cls |
			stream
				cr;
				nextPutAll: cls strategyTitle;
				nextPutAll: ': ';
				nextPutAll: cls strategyDescription ] ]
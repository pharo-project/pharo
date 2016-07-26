user interface
settingStoreOn: aStream
	aStream tab;
		nextPutAll: (self target isSymbol
				ifTrue: [self target]
				ifFalse: [self target name]);
		nextPutAll: (self targetSelector
				ifNil: ['']
				ifNotNil: [' ' , self targetSelector]);
		 nextPutAll: ' ' , self setSelector , ' '.
	self preferenceValue settingStoreOn: aStream.
	aStream nextPut: $.;
		 cr
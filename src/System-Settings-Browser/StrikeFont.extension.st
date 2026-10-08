Extension { #name : 'StrikeFont' }

{ #category : '*System-Settings-Browser' }
StrikeFont >> settingStoreOn: aStream [
	aStream 
		nextPut: $(;
		nextPutAll: self class name;
		nextPut: Character space;
		nextPutAll: 'familyName: ''', self familyName, ''' pointSize: ';
		print: self pointSize; 
		nextPutAll:' emphasized: ';
		print: self emphasis;
		nextPut: $)
]

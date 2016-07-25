*system-settings-browser
settingStoreOn: aStream 
	aStream nextPut: $(.
	aStream nextPutAll: self class name.
	aStream nextPut: Character space.
	aStream nextPutAll: 'familyName: '.
	self familyName settingStoreOn: aStream.
	aStream nextPut: Character space.
	aStream nextPutAll: 'fallbackFamilyNames: '.
	self fallbackFamilyNames settingStoreOn: aStream.
	aStream nextPut: Character space.
	aStream nextPutAll: 'pointSize: '.
	self pointSize settingStoreOn: aStream.
	aStream nextPut: Character space.
	aStream nextPutAll: 'stretchValue: '.
	self stretchValue settingStoreOn: aStream.
	aStream nextPut: Character space.
	aStream nextPutAll: 'weightValue: '.
	self weightValue settingStoreOn: aStream.
	aStream nextPut: Character space.
	aStream nextPutAll: 'slantValue: '.
	self slantValue settingStoreOn: aStream.
	aStream nextPut: $)
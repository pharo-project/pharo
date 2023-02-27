Extension { #name : #LogicalFont }

{ #category : #'*System-Settings-Browser' }
LogicalFont >> acceptSettings: aVisitor [
	^ aVisitor visitLogicalFont: self
]

{ #category : #'*System-Settings-Browser' }
LogicalFont >> settingStoreOn: aStream [
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
]

{ #category : #'*System-Settings-Browser' }
LogicalFont >> withSizeIncrementedBy: anInteger [
	^ self class
		familyName: self familyName
		fallbackFamilyNames: self fallbackFamilyNames
		pointSize: self pointSize + anInteger
		stretchValue: self stretchValue
		weightValue: self weightValue
		slantValue: self slantValue
]

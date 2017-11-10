Extension { #name : #StrikeFontSet }

{ #category : #'*System-Settings' }
StrikeFontSet >> acceptSettings: aVisitor [
	^ aVisitor visitStrikeFontSet: self
]

{ #category : #'*system-settings-browser' }
StrikeFontSet >> settingStoreOn: aStream [ 
	aStream nextPut: $(.
	aStream nextPutAll: self class name.
	aStream nextPut: Character space.
	aStream nextPutAll: 'familyName: ''', self familyName, ''' size: ', self pointSize asString, ' emphasized: ', self emphasis asString.
	aStream nextPut: $)

]

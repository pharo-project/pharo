Extension { #name : #Object }

{ #category : #'*System-Settings' }
Object >> acceptSettings: aVisitor [ 
	^ aVisitor visitObject: self
]

{ #category : #'*System-Settings-Browser' }
Object >> settingFixedDomainValueNodeFrom: aSettingNode [ 
	^ aSettingNode fixedDomainValueNodeForObject: self
]

{ #category : #'*System-Settings-Browser' }
Object >> settingStoreOn: aStream [
	^ self storeOn: aStream
]

Extension { #name : #Object }

{ #category : #'*System-Settings-Browser' }
Object >> settingFixedDomainValueNodeFrom: aSettingNode [
	^ aSettingNode fixedDomainValueNodeForObject: self
]

{ #category : #'*System-Settings-Browser' }
Object >> settingStoreOn: aStream [
	^ self storeOn: aStream
]

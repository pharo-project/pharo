Extension { #name : #Association }

{ #category : #'*System-Settings-Browser' }
Association >> settingFixedDomainValueNodeFrom: aSettingNode [ 
	^ aSettingNode fixedDomainValueNodeForAssociation: self
]

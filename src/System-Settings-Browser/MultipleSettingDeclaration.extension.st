Extension { #name : 'MultipleSettingDeclaration' }

{ #category : '*System-Settings-Browser' }
MultipleSettingDeclaration >> domainValuesLabels [
	^ self domainValues
		collect: [:f | f name]
]

{ #category : '*System-Settings-Browser' }
MultipleSettingDeclaration >> fixedDomainValueNodeForAssociation: anAssociation [
	| s |
	^ (s := (SettingDeclaration new name: anAssociation key translated))
		target: s;
		selector: #default;
		default: anAssociation value
]

{ #category : '*System-Settings-Browser' }
MultipleSettingDeclaration >> fixedDomainValueNodeForObject: anObject [
	| s |
	^ (s := (SettingDeclaration new name: anObject displayString))
		target: s;
		selector: #default;
		default: anObject
]

{ #category : '*System-Settings-Browser' }
MultipleSettingDeclaration >> hasEditableList [
	^ false
]

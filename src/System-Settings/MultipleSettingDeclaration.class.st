"
A declaration for multiple settings
"
Class {
	#name : #MultipleSettingDeclaration,
	#superclass : #SettingDeclaration,
	#instVars : [
		'domainValues'
	],
	#category : #'System-Settings-Core'
}

{ #category : #accessing }
MultipleSettingDeclaration >> domainValues [
	^ domainValues ifNil: [domainValues := OrderedCollection new]
]

{ #category : #accessing }
MultipleSettingDeclaration >> domainValues: aCollection [ 
	domainValues := aCollection asArray collect: [ :v | v settingFixedDomainValueNodeFrom: self]
]

{ #category : #'user interface' }
MultipleSettingDeclaration >> domainValuesLabels [
	^ self domainValues
		collect: [:f | f name]
]

{ #category : #'user interface' }
MultipleSettingDeclaration >> fixedDomainValueNodeForAssociation: anAssociation [ 
	| s |
	^ (s := (SettingDeclaration new name: anAssociation key translated))
		target: s;
		selector: #default;
		default: anAssociation value
]

{ #category : #'user interface' }
MultipleSettingDeclaration >> fixedDomainValueNodeForObject: anObject [ 
	| s |
	^ (s := (SettingDeclaration new name: anObject asString))
		target: s;
		selector: #default;
		default: anObject
]

{ #category : #'user interface' }
MultipleSettingDeclaration >> hasEditableList [
	^ false
]

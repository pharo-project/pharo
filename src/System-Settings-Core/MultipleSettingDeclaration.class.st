"
A declaration for multiple settings
"
Class {
	#name : 'MultipleSettingDeclaration',
	#superclass : 'SettingDeclaration',
	#instVars : [
		'domainValues'
	],
	#category : 'System-Settings-Core-Base',
	#package : 'System-Settings-Core',
	#tag : 'Base'
}

{ #category : 'accessing' }
MultipleSettingDeclaration >> domainValues [
	^ domainValues ifNil: [domainValues := OrderedCollection new]
]

{ #category : 'accessing' }
MultipleSettingDeclaration >> domainValues: aCollection [
	domainValues := aCollection asArray collect: [ :v | v settingFixedDomainValueNodeFrom: self]
]

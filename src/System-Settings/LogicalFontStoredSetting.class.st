"
I am responsible for storing information about LogicalFont objects.
"
Class {
	#name : #LogicalFontStoredSetting,
	#superclass : #AbstractFontStoredSetting,
	#instVars : [
		'fallbackFamilyNames',
		'stretchValue',
		'weightValue',
		'slantValue'
	],
	#category : 'System-Settings-Persistence-Ston-StoredSettings'
}

{ #category : #accessing }
LogicalFontStoredSetting >> fallbackFamilyNames [
	^ fallbackFamilyNames
]

{ #category : #accessing }
LogicalFontStoredSetting >> fallbackFamilyNames: aCollectionOrNil [ 
	fallbackFamilyNames := aCollectionOrNil 
]

{ #category : #accessing }
LogicalFontStoredSetting >> realValue [
	^ (self class environment at: self fontClassName)
				familyName: self familyName 
				fallbackFamilyNames: self fallbackFamilyNames 
				pointSize: self pointSize 
				stretchValue: self stretchValue
				weightValue: self weightValue 
				slantValue: self slantValue
]

{ #category : #accessing }
LogicalFontStoredSetting >> slantValue [
	^ slantValue
]

{ #category : #accessing }
LogicalFontStoredSetting >> slantValue: anInteger [ 
	slantValue := anInteger 
]

{ #category : #accessing }
LogicalFontStoredSetting >> stretchValue [
	^ stretchValue
]

{ #category : #accessing }
LogicalFontStoredSetting >> stretchValue: anInteger [ 
	stretchValue := anInteger
]

{ #category : #accessing }
LogicalFontStoredSetting >> weightValue [
	^ weightValue
]

{ #category : #accessing }
LogicalFontStoredSetting >> weightValue: anInteger [ 
	weightValue := anInteger 
]

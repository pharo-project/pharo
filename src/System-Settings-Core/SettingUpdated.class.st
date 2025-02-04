Class {
	#name : 'SettingUpdated',
	#superclass : 'SystemAnnouncement',
	#instVars : [
		'declaration',
		'newValue',
		'oldValue'
	],
	#category : 'System-Settings-Core-Base',
	#package : 'System-Settings-Core',
	#tag : 'Base'
}

{ #category : 'instance creation' }
SettingUpdated class >> declaration: aSettingDeclaration oldValue: oldValue newValue: newValue [

	^ self new 
		declaration: aSettingDeclaration;
		oldValue: oldValue;
		newValue: newValue;
		yourself
]

{ #category : 'accessing' }
SettingUpdated >> declaration [

	^ declaration
]

{ #category : 'accessing' }
SettingUpdated >> declaration: anObject [

	declaration := anObject
]

{ #category : 'accessing' }
SettingUpdated >> newValue [

	^ newValue
]

{ #category : 'accessing' }
SettingUpdated >> newValue: anObject [

	newValue := anObject
]

{ #category : 'accessing' }
SettingUpdated >> oldValue [

	^ oldValue
]

{ #category : 'accessing' }
SettingUpdated >> oldValue: anObject [

	oldValue := anObject
]

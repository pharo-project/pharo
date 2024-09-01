Class {
	#name : 'ActionSettingDeclaration',
	#superclass : 'SettingDeclaration',
	#instVars : [
		'actionLabel'
	],
	#category : 'System-Settings-Core-Base',
	#package : 'System-Settings-Core',
	#tag : 'Base'
}

{ #category : 'accessing' }
ActionSettingDeclaration >> actionLabel [

	^ actionLabel
]

{ #category : 'accessing' }
ActionSettingDeclaration >> actionLabel: anObject [

	actionLabel := anObject
]

{ #category : 'user interface' }
ActionSettingDeclaration >> inputWidget [
	"Private - Morphic requirement. To be deleted when morphic gets removed"

	^ SimpleButtonMorph new
		target: self target;
		label: self label;
		actionSelector: self name;
		themeChanged;
		yourself
]

{ #category : 'accessing' }
ActionSettingDeclaration >> realValue [
	"Private - Morphic requirement. To be deleted when morphic gets removed"

	^ false
]

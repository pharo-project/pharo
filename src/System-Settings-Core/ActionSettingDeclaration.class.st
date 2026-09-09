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

{ #category : 'accessing' }
ActionSettingDeclaration >> realValue [
	"Private - Morphic requirement. To be deleted when morphic gets removed"

	^ false
]

{ #category : 'accessing' }
ActionSettingDeclaration >> realValue: anObject [
	"Private - Do nothing, to prevent raising an Error in updateSettingNode:"
]

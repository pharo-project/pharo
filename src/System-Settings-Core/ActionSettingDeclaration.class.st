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
ActionSettingDeclaration >> hasValue [
	"An action setting (a button) holds no persistable value — it triggers a
	 side-effecting action when invoked. It must never be included in an
	 exported startup script, and its 'current value' must never be computed
	 by actually invoking the action (see #19869)."

	^ false
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

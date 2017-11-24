"
I represent a shortcut declaration for the settings pharo framework.
"
Class {
	#name : #KMShortcutDeclaration,
	#superclass : #SettingDeclaration,
	#instVars : [
		'categoryName',
		'declaration',
		'shortcutName'
	],
	#category : #'Keymapping-Settings'
}

{ #category : #accessing }
KMShortcutDeclaration >> categoryName [
	^categoryName
]

{ #category : #accessing }
KMShortcutDeclaration >> categoryName: aCategoryName [
	categoryName := aCategoryName
]

{ #category : #accessing }
KMShortcutDeclaration >> defaultValue [
	^self realValue defaultShortcut
]

{ #category : #accessing }
KMShortcutDeclaration >> realValue [
	"Answer the value of realValue"

	^ declaration
		ifNil: [
			declaration := KMShortcutSetting for: self shortcutName in: self categoryName]
]

{ #category : #accessing }
KMShortcutDeclaration >> realValue: aShortcut [
	self realValue accept: aShortcut.
]

{ #category : #accessing }
KMShortcutDeclaration >> shortcutName [
	^shortcutName
]

{ #category : #accessing }
KMShortcutDeclaration >> shortcutName: aShortcutName [
	shortcutName := aShortcutName
]

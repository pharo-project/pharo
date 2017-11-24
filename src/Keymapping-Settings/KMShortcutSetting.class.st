"
I represent a shortcut setting. I relate a keymap with its default shortcut and its current shortcut, so it can be mutated.
"
Class {
	#name : #KMShortcutSetting,
	#superclass : #Object,
	#instVars : [
		'shortcutName',
		'category',
		'action'
	],
	#category : #'Keymapping-Settings'
}

{ #category : #'instance creation' }
KMShortcutSetting class >> for: aShortcutName in: aCategory [
	^ (self new)
		shortcutName: aShortcutName;
		category: aCategory;
		yourself
]

{ #category : #settings }
KMShortcutSetting class >> settingInputWidgetForNode: aSettingNode [
	| catcherMorph theme |
	theme := UITheme builder.
	catcherMorph := KMCatcherMorph for: aSettingNode realValue.
	^ theme newRow: {catcherMorph}
]

{ #category : #setting }
KMShortcutSetting >> accept: aShortcut [ 
	self keymap shortcut: aShortcut
]

{ #category : #accessing }
KMShortcutSetting >> action: anActionBlock [
	action := anActionBlock
]

{ #category : #accessing }
KMShortcutSetting >> category: aCategory [
	category := aCategory
]

{ #category : #accessing }
KMShortcutSetting >> defaultShortcut [
	^ self keymap defaultShortcut
]

{ #category : #accessing }
KMShortcutSetting >> keymap [
	^ KMFactory keymapContainer keymapNamed: shortcutName inCategory: category
]

{ #category : #accessing }
KMShortcutSetting >> keystrokes [
	^ self keymap shortcut
]

{ #category : #accessing }
KMShortcutSetting >> shortcut [
	^ self keymap shortcut
]

{ #category : #accessing }
KMShortcutSetting >> shortcutName: aShortcutName [
	shortcutName := aShortcutName
]

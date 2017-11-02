"
I am a menu item morph with the ability to refresh periodically all my values (name, state, enablement, icon, submenu)

Since it is not a good idea to change the shortcut, this ability is not available
"
Class {
	#name : #UpdatingMenuItemMorph,
	#superclass : #ToggleMenuItemMorph,
	#instVars : [
		'nameSelector',
		'iconSelector',
		'subMenuSelector',
		'balloonTextSelector'
	],
	#category : 'Morphic-Base-Menus'
}

{ #category : #accessing }
UpdatingMenuItemMorph >> balloonText [

	^ self target perform: balloonTextSelector
]

{ #category : #accessing }
UpdatingMenuItemMorph >> balloonTextSelector [
	^ balloonTextSelector
]

{ #category : #accessing }
UpdatingMenuItemMorph >> balloonTextSelector: anObject [
	balloonTextSelector := anObject.
	self updateBalloonText
]

{ #category : #accessing }
UpdatingMenuItemMorph >> contents [

	^ self target perform: nameSelector
]

{ #category : #accessing }
UpdatingMenuItemMorph >> icon [

	^ (target perform: iconSelector)
		ifNil: [ super icon ]
]

{ #category : #accessing }
UpdatingMenuItemMorph >> iconSelector [
	^ iconSelector
]

{ #category : #accessing }
UpdatingMenuItemMorph >> iconSelector: anObject [
	iconSelector := anObject.
	self updateIcon
]

{ #category : #accessing }
UpdatingMenuItemMorph >> nameSelector [
	^ nameSelector
]

{ #category : #accessing }
UpdatingMenuItemMorph >> nameSelector: aSymbol [
	nameSelector := aSymbol.
	self updateContents
]

{ #category : #stepping }
UpdatingMenuItemMorph >> step [

	owner ifNil: [ ^ self stopStepping ].
	self updateContents.
	self changed
]

{ #category : #stepping }
UpdatingMenuItemMorph >> stepTime [
	
	^ 250
]

{ #category : #accessing }
UpdatingMenuItemMorph >> subMenuSelector [
	^ subMenuSelector
]

{ #category : #accessing }
UpdatingMenuItemMorph >> subMenuSelector: anObject [
	subMenuSelector := anObject.
	self updateSubMenu
]

{ #category : #private }
UpdatingMenuItemMorph >> updateBalloonText [

	target ifNotNil: [ self balloonText: (target perform: balloonTextSelector) ]
]

{ #category : #private }
UpdatingMenuItemMorph >> updateContents [

	target ifNotNil: [ contents := (target perform: nameSelector) ]
]

{ #category : #private }
UpdatingMenuItemMorph >> updateIcon [

	target ifNotNil: [ self icon: (target perform: iconSelector) ]
]

{ #category : #private }
UpdatingMenuItemMorph >> updateSubMenu [

	target ifNotNil: [ subMenu := (target perform: subMenuSelector) ]
]

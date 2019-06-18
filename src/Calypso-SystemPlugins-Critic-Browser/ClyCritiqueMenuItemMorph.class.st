"
I simulate the functionality of MenuItemMorph while providing more sophisticated visualization for rubric annotations 
"
Class {
	#name : #ClyCritiqueMenuItemMorph,
	#superclass : #Morph,
	#category : #'Calypso-SystemPlugins-Critic-Browser'
}

{ #category : #events }
ClyCritiqueMenuItemMorph >> activateSubmenu: evt [

	^ false
]

{ #category : #selecting }
ClyCritiqueMenuItemMorph >> deselect: sth [
]

{ #category : #accessing }
ClyCritiqueMenuItemMorph >> hasIconOrMarker [
	^ true
]

{ #category : #accessing }
ClyCritiqueMenuItemMorph >> hasSubMenu [

	^ false
]

{ #category : #accessing }
ClyCritiqueMenuItemMorph >> isEnabled [

	^ true
]

{ #category : #testing }
ClyCritiqueMenuItemMorph >> isMenuItemMorph [
	^ true
]

{ #category : #selecting }
ClyCritiqueMenuItemMorph >> select: sth [
]

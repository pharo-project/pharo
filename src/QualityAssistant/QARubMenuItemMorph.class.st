"
I simulate the functionality of MenuItemMorph while providing more sophisticated visualization for rubric annotations 
"
Class {
	#name : #QARubMenuItemMorph,
	#superclass : #Morph,
	#category : 'QualityAssistant'
}

{ #category : #events }
QARubMenuItemMorph >> activateSubmenu: evt [

	^ false
]

{ #category : #selecting }
QARubMenuItemMorph >> deselect: sth [
]

{ #category : #accessing }
QARubMenuItemMorph >> hasIconOrMarker [
	^ true
]

{ #category : #accessing }
QARubMenuItemMorph >> hasSubMenu [

	^ false
]

{ #category : #accessing }
QARubMenuItemMorph >> isEnabled [

	^ true
]

{ #category : #testing }
QARubMenuItemMorph >> isMenuItemMorph [
	^ true
]

{ #category : #selecting }
QARubMenuItemMorph >> select: sth [
]

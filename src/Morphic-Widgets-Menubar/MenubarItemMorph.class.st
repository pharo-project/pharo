Class {
	#name : #MenubarItemMorph,
	#superclass : #DockingBarMenuItemMorph,
	#category : #'Morphic-Widgets-Menubar'
}

{ #category : #drawing }
MenubarItemMorph >> drawSubmenuMarkerOn: aCanvas [

	"Nothing"
]

{ #category : #drawing }
MenubarItemMorph >> minHeight [

	^ super minHeight + 6
]

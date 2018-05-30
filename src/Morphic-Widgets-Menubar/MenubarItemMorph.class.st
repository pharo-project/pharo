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
MenubarItemMorph >> menuStringBounds [

	| oldBounds |
	oldBounds := super menuStringBounds.
	^ oldBounds left: (oldBounds left + oldBounds right - self measureContents x) // 2
]

{ #category : #drawing }
MenubarItemMorph >> minHeight [

	^ super minHeight + 6
]

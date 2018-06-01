Class {
	#name : #MenubarItemMorph,
	#superclass : #DockingBarMenuItemMorph,
	#category : #'Morphic-Widgets-Menubar'
}

{ #category : #drawing }
MenubarItemMorph >> drawIconOn: aCanvas [
	| iconForm |
	self hasIcon ifFalse: [ ^ self ].
	
	iconForm := self iconForm.
	aCanvas translucentImage: iconForm at: (self menuStringBounds left - iconForm width - 5 ) @ (self top + ((self height - iconForm height) // 2))
]

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

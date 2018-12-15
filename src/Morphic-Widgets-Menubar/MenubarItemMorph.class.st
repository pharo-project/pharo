"
Description
--------------------

I am a specialization of the MenuItemMorph for the Menubar. I adapt the look of the Morph to fit the design of the menubar.
"
Class {
	#name : #MenubarItemMorph,
	#superclass : #DockingBarMenuItemMorph,
	#category : #'Morphic-Widgets-Menubar-View'
}

{ #category : #drawing }
MenubarItemMorph >> drawIconOn: aCanvas [
	| iconForm x y |
	self hasIcon ifFalse: [ ^ self ].
	
	iconForm := self iconForm.
	x := (self menuStringBounds left - iconForm width - 5 ).
	y := (self top + ((self height - iconForm height) // 2)).
	
	aCanvas translucentImage: iconForm at: x @ y
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

{ #category : #private }
MenubarItemMorph >> selectionFillStyle [
	^ self theme menubarItemSelectionFillStyleFor: self
]

{ #category : #private }
MenubarItemMorph >> selectionTextColor [
	^ self theme menubarItemSelectionTextColorFor: self
]

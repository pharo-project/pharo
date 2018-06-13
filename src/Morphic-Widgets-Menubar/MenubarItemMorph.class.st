Class {
	#name : #MenubarItemMorph,
	#superclass : #DockingBarMenuItemMorph,
	#category : #'Morphic-Widgets-Menubar'
}

{ #category : #drawing }
MenubarItemMorph >> drawIconOn: aCanvas [
	| iconForm x y |
	self hasIcon ifFalse: [ ^ self ].
	
	iconForm := self iconForm.
	x := (self menuStringBounds left - iconForm width - 5 ).
	y := (self top + ((self height - iconForm height) // 2)).
	
	self shouldBeHighlighted ifTrue: [ y := y + 2 ].
	
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
	self shouldBeHighlighted ifTrue: [ oldBounds := oldBounds top: oldBounds top + 2 ].
	^ oldBounds left: (oldBounds left + oldBounds right - self measureContents x) // 2
]

{ #category : #drawing }
MenubarItemMorph >> minHeight [

	^ super minHeight + 6
]

{ #category : #private }
MenubarItemMorph >> selectionFillStyle [
	^ self theme menubarItemSelectionFillStyleFor: self
]

{ #category : #private }
MenubarItemMorph >> selectionTextColor [
	^ self theme menubarItemSelectionTextColorFor: self
]

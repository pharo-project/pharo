Class {
	#name : #MenubarMenuMorph,
	#superclass : #MenuMorph,
	#category : #'Morphic-Widgets-Menubar'
}

{ #category : #events }
MenubarMenuMorph >> leftArrowStroked: evt [
	| keyboardFocus |
	super leftArrowStroked: evt.
	
	"If the menu is a submenu of a MenubarItemMorph then we need to give back the focus but also move the MenubarItemMorph selected one left."
	keyboardFocus := evt hand keyboardFocus.
	keyboardFocus isMenubar ifTrue: [ keyboardFocus keyStroke: evt ]
]

{ #category : #events }
MenubarMenuMorph >> rightArrowStroked: evt [
	"If the super return false it's probably because we need to give back the focus to the menubar and move to the right."

	(super rightArrowStroked: evt)
		ifTrue: [ ^ true ].
	popUpOwner
		ifNotNil: [ self deselectAndFocusOutermenuOn: evt.
			self sendRightArrowPressedToMenubarOwner: evt.
			^ true ].
	^ false
]

{ #category : #events }
MenubarMenuMorph >> sendRightArrowPressedToMenubarOwner: evt [
	popUpOwner owner isMenubar
		ifTrue:
			[ "This is a submenu of a MenubarItemMorph, we give back the focus and move one right." popUpOwner owner keyStroke: evt ]
		ifFalse:
			[ "In that case we have submenus in submenus, we delegate the action to our parent menu" popUpOwner owner sendRightArrowPressedToMenubarOwner: evt ]
]

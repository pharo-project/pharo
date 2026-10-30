"
I am the base of the context menu openers of Calypso and SystemCommands. I speak the Commander2 vocabulary (name and iconName) and answer the Commander v1 names (defaultMenuItemName and defaultMenuIconName) from it, so both APIs keep working until Commander v1 is removed. My menu building is still the Commander v1 one.
"
Class {
	#name : 'ClyOpenContextMenuCommand',
	#superclass : 'CmdOpenContextMenuCommand',
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'testing' }
ClyOpenContextMenuCommand class >> isAbstract [
	^ self = ClyOpenContextMenuCommand
]

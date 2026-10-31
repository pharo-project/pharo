"
I am the common base of the commands of Calypso and SystemCommands. I speak the Commander2 vocabulary (name and iconName) and answer the Commander v1 names (defaultMenuItemName and defaultMenuIconName) from it, so both APIs keep working until Commander v1 is removed.
"
Class {
	#name : 'ClyCommand',
	#superclass : 'CmdCommand',
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'testing' }
ClyCommand class >> isAbstract [

	^ self = ClyCommand
]

{ #category : 'accessing' }
ClyCommand >> defaultMenuIconName [
	"Temporary method for Commander2 migration."

	^ self iconName
]

{ #category : 'accessing' }
ClyCommand >> defaultMenuItemName [
	"Temporary method for Commander2 migration."

	^ self name
]

{ #category : 'accessing' }
ClyCommand >> iconName [
	"Temporary method for Commander2 migration."

	^ super defaultMenuIconName
]

{ #category : 'accessing' }
ClyCommand >> name [
	"Temporary method for Commander2 migration."

	^ super defaultMenuItemName
]

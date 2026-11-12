"
I am the base of the groups which place Calypso commands into a toolbar. Commands are put in a toolbar group by their declaration:
	(aCommandMap toolbar group: ClyPackageToolbarGroup) add: ClySwitchToPackagesCommand order: 1
The toolbar orders the groups by their order and name and places the commands of each group next to each other. See the guide doc/Calypso/Commands.md of the Pharo repository.
"
Class {
	#name : 'ClyToolbarGroup',
	#superclass : 'Object',
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'presenting' }
ClyToolbarGroup >> isInlined [
	"Toolbar groups are never drawn inline in a menu."
	^ false
]

{ #category : 'accessing' }
ClyToolbarGroup >> name [
	^ self class name
]

{ #category : 'accessing' }
ClyToolbarGroup >> order [
	^ 500
]

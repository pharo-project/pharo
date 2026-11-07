"
I am the base of the groups which place Calypso commands into a toolbar. My subclasses are used as the menu group of a toolbar activation. ClyToolbarBuilder orders the groups by their order and name and places the commands of each group into the toolbar.
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

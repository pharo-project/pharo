"
Opens the Extras submenu of the variable and inherited method group menus, filled with the method group Extras commands.
"
Class {
	#name : 'SycOpenExtraInMethodGroupMenuCommand',
	#superclass : 'SycOpenContextMenuCommand',
	#category : 'Calypso-SystemTools-FullBrowser',
	#package : 'Calypso-SystemTools-FullBrowser'
}

{ #category : 'accessing' }
SycOpenExtraInMethodGroupMenuCommand >> activationStrategy [

	^ SycExtraMenuActivation
]

{ #category : 'accessing' }
SycOpenExtraInMethodGroupMenuCommand >> cmCommandClass [

	^ SycMethodGroupExtraCmCommand
]

{ #category : 'accessing' }
SycOpenExtraInMethodGroupMenuCommand >> iconName [

	^ #smallRedo
]

{ #category : 'accessing' }
SycOpenExtraInMethodGroupMenuCommand >> name [

	^ 'Extras'
]

{ #category : 'menu' }
SycOpenExtraInMethodGroupMenuCommand >> pureMenuCommands [
	"The commands of the class of this opener that apply here, in the order they are built."
	^ self classFamilyPureMenuCommands
]

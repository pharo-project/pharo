"
Opens the Extras submenu of the package and tag browser menus, filled with the package Extras commands.
"
Class {
	#name : 'SycOpenExtraInPackageMenuCommand',
	#superclass : 'SycOpenContextMenuCommand',
	#category : 'SystemCommands-PackageCommands',
	#package : 'SystemCommands-PackageCommands'
}

{ #category : 'accessing' }
SycOpenExtraInPackageMenuCommand >> activationStrategy [

	^ SycExtraMenuActivation
]

{ #category : 'accessing' }
SycOpenExtraInPackageMenuCommand >> cmCommandClass [

	^ SycPackageExtraCmCommand
]

{ #category : 'accessing' }
SycOpenExtraInPackageMenuCommand >> iconName [

	^ #smallRedo
]

{ #category : 'accessing' }
SycOpenExtraInPackageMenuCommand >> name [

	^ 'Extras'
]

{ #category : 'menu' }
SycOpenExtraInPackageMenuCommand >> pureMenuCommands [
	"The commands of the class of this opener that apply here, in the order they are built."
	^ self classFamilyPureMenuCommands
]

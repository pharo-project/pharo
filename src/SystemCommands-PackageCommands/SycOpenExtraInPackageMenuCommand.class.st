"
I open the Extras submenu of the package and tag browser menus, filled with the package Extras commands.
"
Class {
	#name : 'SycOpenExtraInPackageMenuCommand',
	#superclass : 'SycOpenContextMenuCommand',
	#category : 'SystemCommands-PackageCommands',
	#package : 'SystemCommands-PackageCommands'
}

{ #category : 'activation' }
SycOpenExtraInPackageMenuCommand class >> packageContextMenuActivation [
	<classAnnotation>

	^ CmdContextMenuActivation byRootGroupItemOrder: 10200 for: Package asCalypsoItemContext
]

{ #category : 'accessing' }
SycOpenExtraInPackageMenuCommand >> activationStrategy [
	^ SycExtraMenuActivation
]

{ #category : 'execution' }
SycOpenExtraInPackageMenuCommand >> addClyCommandsToMenu: aMenuMorph [
	"Temporary: draws the commands sorted by name until the group builder sorts its entries."
	| group renderer |
	group := ClyCommandGroupBuilder
		buildFor: self cmCommandClass
		context: self context.
	renderer := ClyCommandGroupMenuRenderer new.
	(group commands asSortedCollection: [ :a :b | a name <= b name ])
		do: [ :each | renderer renderCommand: each on: aMenuMorph ].
	^ aMenuMorph
]

{ #category : 'accessing' }
SycOpenExtraInPackageMenuCommand >> cmCommandClass [
	^ SycPackageExtraCmCommand
]

{ #category : 'accessing' }
SycOpenExtraInPackageMenuCommand >> name [
	^ 'Extra'
]

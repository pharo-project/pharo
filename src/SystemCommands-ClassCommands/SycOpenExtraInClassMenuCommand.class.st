Class {
	#name : 'SycOpenExtraInClassMenuCommand',
	#superclass : 'SycOpenContextMenuCommand',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'activation' }
SycOpenExtraInClassMenuCommand class >> browserContextMenuActivation [
	<classAnnotation>

	^ CmdContextMenuActivation 
		byRootGroupItemOrder: 10200 
		for: ClyClass asCalypsoItemContext
]

{ #category : 'execution' }
SycOpenExtraInClassMenuCommand >> activationStrategy [
	^ SycExtraMenuActivation
]

{ #category : 'execution' }
SycOpenExtraInClassMenuCommand >> addClyCommandsToMenu: aMenuMorph [

	| group |
	group := ClyCommandGroupBuilder
		         buildFor: self cmCommandClass
		         context: self context.
	^ ClyCommandGroupMenuRenderer
		  render: group
		  on: aMenuMorph
]

{ #category : 'context menu' }
SycOpenExtraInClassMenuCommand >> cmCommandClass [
	^ SycClassExtraCmCommand
]

{ #category : 'accessing' }
SycOpenExtraInClassMenuCommand >> defaultMenuIconName [
	^ #smallRedo
]

{ #category : 'accessing' }
SycOpenExtraInClassMenuCommand >> defaultMenuItemName [

	^ 'Extras'
]

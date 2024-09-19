Class {
	#name : 'SycOpenExtraInClassMenuCommand',
	#superclass : 'SycOpenContextMenuCommand',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'activation' }
SycOpenExtraInClassMenuCommand class >> browserContextMenuActivation [
	<classAnnotation>

	^ CmdContextMenuActivation byRootGroupItemOrder: 1.56 for: ClyClass asCalypsoItemContext
]

{ #category : 'execution' }
SycOpenExtraInClassMenuCommand >> activationStrategy [
	^ SycExtraMenuActivation
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

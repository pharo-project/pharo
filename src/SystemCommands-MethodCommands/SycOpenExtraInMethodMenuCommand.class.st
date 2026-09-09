Class {
	#name : 'SycOpenExtraInMethodMenuCommand',
	#superclass : 'SycOpenContextMenuCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'activation' }
SycOpenExtraInMethodMenuCommand class >> methodContextMenuActivation [

	<classAnnotation>
	
	^ CmdContextMenuActivation
		byItemOf: CmdRootMenuGroup 
		order: 11200 
		for: ClyMethod asCalypsoItemContext
]

{ #category : 'execution' }
SycOpenExtraInMethodMenuCommand >> activationStrategy [

	^ SycExtraMenuActivation 
]

{ #category : 'context menu' }
SycOpenExtraInMethodMenuCommand >> cmCommandClass [
	^ SycMethodExtraCmCommand
]

{ #category : 'accessing' }
SycOpenExtraInMethodMenuCommand >> defaultMenuIconName [

	^ #forward 
]

{ #category : 'accessing' }
SycOpenExtraInMethodMenuCommand >> defaultMenuItemName [

	^ 'Extra' 
]

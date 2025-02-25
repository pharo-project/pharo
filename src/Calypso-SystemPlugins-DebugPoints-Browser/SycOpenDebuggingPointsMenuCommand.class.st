"
I define how the debugging point menu group should activate inside calypso
"
Class {
	#name : 'SycOpenDebuggingPointsMenuCommand',
	#superclass : 'SycOpenContextMenuCommand',
	#category : 'Calypso-SystemPlugins-DebugPoints-Browser',
	#package : 'Calypso-SystemPlugins-DebugPoints-Browser'
}

{ #category : 'activation' }
SycOpenDebuggingPointsMenuCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 0.2 for: ClySourceCodeContext
]

{ #category : 'execution' }
SycOpenDebuggingPointsMenuCommand >> activationStrategy [

	^ SycDebuggingPointsMenuActivation
]

{ #category : 'context menu' }
SycOpenDebuggingPointsMenuCommand >> cmCommandClass [

	^ SycSourceCodeCmCommand
]

{ #category : 'accessing' }
SycOpenDebuggingPointsMenuCommand >> defaultMenuIconName [
	^ #smallDebug
]

{ #category : 'accessing' }
SycOpenDebuggingPointsMenuCommand >> defaultMenuItemName [

	^ 'Debug Points'
]

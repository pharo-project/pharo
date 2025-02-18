"
I define how the debugging point menu group should activate inside the calypso method view
"
Class {
	#name : 'SycOpenDebuggingPointsInMethodMenuCommand',
	#superclass : 'SycOpenMethodMenuCommand',
	#category : 'Calypso-SystemPlugins-DebugPoints-Browser',
	#package : 'Calypso-SystemPlugins-DebugPoints-Browser'
}

{ #category : 'activation' }
SycOpenDebuggingPointsInMethodMenuCommand class >> methodContextMenuActivation [

	<classAnnotation>
	^ CmdContextMenuActivation
		  byRootGroupItemOrder: 1.4
		  for: ClyMethod asCalypsoItemContext 
]

{ #category : 'execution' }
SycOpenDebuggingPointsInMethodMenuCommand >> activationStrategy [

	^ SycDebuggingPointsMenuActivation 
]

{ #category : 'accessing' }
SycOpenDebuggingPointsInMethodMenuCommand >> defaultMenuIconName [

	^ #smallDebug 
]

{ #category : 'accessing' }
SycOpenDebuggingPointsInMethodMenuCommand >> defaultMenuItemName [

	^ 'Debug Points' 
]

{ #category : 'accessing' }
SycOpenDebuggingPointsInMethodMenuCommand >> description [

	^ 'Defines how the debugging point menu group should activate inside the calypso method view.'
]

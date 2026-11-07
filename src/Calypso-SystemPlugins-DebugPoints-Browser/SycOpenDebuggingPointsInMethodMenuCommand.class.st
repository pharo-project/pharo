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
	^ ClyContextMenuActivation
		  byRootGroupItemOrder: 1.4
		  for: ClyMethod asCalypsoItemContext 
]

{ #category : 'execution' }
SycOpenDebuggingPointsInMethodMenuCommand >> activationStrategy [

	^ SycDebuggingPointsMenuActivation 
]

{ #category : 'accessing' }
SycOpenDebuggingPointsInMethodMenuCommand >> description [

	^ 'Defines how the debugging point menu group should activate inside the calypso method view.'
]

{ #category : 'accessing' }
SycOpenDebuggingPointsInMethodMenuCommand >> iconName [

	^ #smallDebug 
]

{ #category : 'accessing' }
SycOpenDebuggingPointsInMethodMenuCommand >> name [

	^ 'Debug Points' 
]

{ #category : 'menu' }
SycOpenDebuggingPointsInMethodMenuCommand >> pureMenuCommands [
	"Its sub-menu holds debugging points only: no pure commands."
	^ #()
]

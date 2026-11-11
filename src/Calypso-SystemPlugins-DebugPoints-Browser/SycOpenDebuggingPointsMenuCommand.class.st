"
I define how the debugging point menu group should activate inside calypso
"
Class {
	#name : 'SycOpenDebuggingPointsMenuCommand',
	#superclass : 'SycOpenContextMenuCommand',
	#category : 'Calypso-SystemPlugins-DebugPoints-Browser',
	#package : 'Calypso-SystemPlugins-DebugPoints-Browser'
}

{ #category : 'execution' }
SycOpenDebuggingPointsMenuCommand >> activationStrategy [

	^ SycDebuggingPointsMenuActivation
]

{ #category : 'context menu' }
SycOpenDebuggingPointsMenuCommand >> cmCommandClass [

	^ SycSourceCodeCmCommand
]

{ #category : 'accessing' }
SycOpenDebuggingPointsMenuCommand >> iconName [
	^ #smallDebug
]

{ #category : 'accessing' }
SycOpenDebuggingPointsMenuCommand >> name [

	^ 'Debug Points'
]

"
I am the ""Debug Points"" sub-menu of the context menu of methods. Meta+T opens my commands as a pop-up menu (ClyOpenMenuGroupCommand).
"
Class {
	#name : 'ClyMethodDebugPointsMenuGroup',
	#superclass : 'ClyMenuGroup',
	#category : 'Calypso-SystemPlugins-DebugPoints-Browser',
	#package : 'Calypso-SystemPlugins-DebugPoints-Browser'
}

{ #category : 'accessing' }
ClyMethodDebugPointsMenuGroup >> description [

	^ 'Defines how the debugging point menu group should activate inside the calypso method view.'
]

{ #category : 'accessing' }
ClyMethodDebugPointsMenuGroup >> iconName [

	^ #smallDebug
]

{ #category : 'accessing' }
ClyMethodDebugPointsMenuGroup >> name [

	^ 'Debug Points'
]

{ #category : 'accessing' }
ClyMethodDebugPointsMenuGroup >> order [

	^ 1.4
]

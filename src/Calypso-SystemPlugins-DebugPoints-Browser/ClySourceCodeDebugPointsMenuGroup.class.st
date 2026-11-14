"
I am the ""Debug Points"" sub-menu of the context menu of method editors.
"
Class {
	#name : 'ClySourceCodeDebugPointsMenuGroup',
	#superclass : 'ClyMenuGroup',
	#category : 'Calypso-SystemPlugins-DebugPoints-Browser',
	#package : 'Calypso-SystemPlugins-DebugPoints-Browser'
}

{ #category : 'accessing' }
ClySourceCodeDebugPointsMenuGroup >> iconName [

	^ #smallDebug
]

{ #category : 'accessing' }
ClySourceCodeDebugPointsMenuGroup >> name [

	^ 'Debug Points'
]

{ #category : 'accessing' }
ClySourceCodeDebugPointsMenuGroup >> order [

	^ 0.2
]

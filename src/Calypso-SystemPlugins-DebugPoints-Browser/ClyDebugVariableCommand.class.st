"
I am the base class to install debug points on a variable on the calypso browser
"
Class {
	#name : 'ClyDebugVariableCommand',
	#superclass : 'SycVariableCommand',
	#category : 'Calypso-SystemPlugins-DebugPoints-Browser',
	#package : 'Calypso-SystemPlugins-DebugPoints-Browser'
}

{ #category : 'accessing' }
ClyDebugVariableCommand class >> accessStrategy [

	self subclassResponsibility
]

{ #category : 'accessing' }
ClyDebugVariableCommand class >> debugPointClass [

	^ DebugPoint
]

{ #category : 'activation' }
ClyDebugVariableCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	^ CmdContextMenuActivation
		byItemOf: ClyDebuggingPointsMenuGroup
		for: ClyFullBrowserVariableContext
]

{ #category : 'testing' }
ClyDebugVariableCommand class >> isAbstract [

	^ true
]

{ #category : 'accessing' }
ClyDebugVariableCommand >> accessStrategy [

	^ self class accessStrategy
]

{ #category : 'accessing' }
ClyDebugVariableCommand >> debugPointClass [

	^ self class debugPointClass
]

{ #category : 'accessing' }
ClyDebugVariableCommand >> defaultMenuIconName [

	^ #smallDebug
]

{ #category : 'accessing' }
ClyDebugVariableCommand >> defaultMenuItemName [

	^ 'Add debug point:'
]

{ #category : 'execution' }
ClyDebugVariableCommand >> execute [

	variables do: [ :var | self executeFor: var ]
]

{ #category : 'private execution' }
ClyDebugVariableCommand >> executeFor: aVariable [

	DebugPointManager
		installNew: self debugPointClass
		inClass: aVariable definingClass
		onVariableNamed: aVariable name
		accessStrategy: self accessStrategy
]

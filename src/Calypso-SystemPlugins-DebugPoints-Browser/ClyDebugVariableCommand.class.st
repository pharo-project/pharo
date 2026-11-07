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
	^ ClyContextMenuActivation
		byItemOf: ClyDebuggingPointsMenuGroup
		for: ClyFullBrowserVariableContext
]

{ #category : 'accessing' }
ClyDebugVariableCommand >> accessStrategy [

	^ self class accessStrategy
]

{ #category : 'accessing' }
ClyDebugVariableCommand >> debugPointClass [

	^ self class debugPointClass
]

{ #category : 'private execution' }
ClyDebugVariableCommand >> executeFor: aVariable [

	DebugPointManager
		installNew: self debugPointClass
		inClass: aVariable definingClass
		onVariableNamed: aVariable name
		accessStrategy: self accessStrategy
]

{ #category : 'execution' }
ClyDebugVariableCommand >> executeRefactoring [

	variables do: [ :var | self executeFor: var ]
]

{ #category : 'accessing' }
ClyDebugVariableCommand >> iconName [

	^ #smallDebug
]

{ #category : 'accessing' }
ClyDebugVariableCommand >> name [

	^ 'Add debug point:'
]

{ #category : 'execution' }
ClyDebugVariableCommand >> prepareFullExecution [

	variables := self context selectedVariables
]

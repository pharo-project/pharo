"
I am a command to inspect given variable.

"
Class {
	#name : 'SycInspectVariableCommand',
	#superclass : 'SycMethodGroupExtraCmCommand',
	#category : 'SystemCommands-VariableCommands',
	#package : 'SystemCommands-VariableCommands'
}

{ #category : 'accessing' }
SycInspectVariableCommand >> description [

	^ 'Inspect variable'
]

{ #category : 'execution' }
SycInspectVariableCommand >> executeRefactoring [

	self context selectedVariables last openInspector
]

{ #category : 'accessing' }
SycInspectVariableCommand >> icon [

	^ self iconNamed: #inspect
]

{ #category : 'accessing' }
SycInspectVariableCommand >> iconName [

	^ #inspect
]

{ #category : 'testing' }
SycInspectVariableCommand >> isApplicable [

	^ context isAboutBrowserItemType: ClyVariableMethodGroup
]

{ #category : 'accessing' }
SycInspectVariableCommand >> name [

	^ 'Inspect variable'
]

{ #category : 'accessing' }
SycInspectVariableCommand >> order [

	^ 1000
]

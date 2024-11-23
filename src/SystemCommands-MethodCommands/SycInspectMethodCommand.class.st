"
Inspect the selected methods
"
Class {
	#name : 'SycInspectMethodCommand',
	#superclass : 'SycMethodExtraCmCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'accessing' }
SycInspectMethodCommand >> description [

	^ 'Inspect the selected methods'
]

{ #category : 'executing' }
SycInspectMethodCommand >> execute [

	self methods do: #inspect
]

{ #category : 'accessing' }
SycInspectMethodCommand >> icon [

	^ self iconNamed: #inspect
]

{ #category : 'accessing' }
SycInspectMethodCommand >> name [

	^ 'Inspect'
]

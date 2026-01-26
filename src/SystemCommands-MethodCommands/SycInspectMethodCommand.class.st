"
Inspect the selected methods
"
Class {
	#name : 'SycInspectMethodCommand',
	#superclass : 'SycMethodExtraCmCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'executing' }
SycInspectMethodCommand >> applyCommandResult [

	self methods do: #inspect
]

{ #category : 'accessing' }
SycInspectMethodCommand >> description [

	^ 'Inspect the selected methods'
]

{ #category : 'accessing' }
SycInspectMethodCommand >> icon [

	^ self iconNamed: #inspect
]

{ #category : 'accessing' }
SycInspectMethodCommand >> name [

	^ 'Inspect'
]

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

{ #category : 'execution' }
SycInspectMethodCommand >> execute [

	self methods do: [ :method | method inspect ]
]

{ #category : 'accessing' }
SycInspectMethodCommand >> iconName [

	^ #inspect
]

{ #category : 'accessing' }
SycInspectMethodCommand >> name [

	^ 'Inspect'
]

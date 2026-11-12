"
Inspect the selected methods
"
Class {
	#name : 'SycInspectMethodCommand',
	#superclass : 'SycMethodCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

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

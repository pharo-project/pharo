"
I am the ""Extra"" sub-menu of the context menu of methods.
"
Class {
	#name : 'SycMethodExtraMenuGroup',
	#superclass : 'ClyMenuGroup',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'accessing' }
SycMethodExtraMenuGroup >> iconName [

	^ #forward
]

{ #category : 'accessing' }
SycMethodExtraMenuGroup >> name [

	^ 'Extra'
]

{ #category : 'accessing' }
SycMethodExtraMenuGroup >> order [

	^ 11200
]

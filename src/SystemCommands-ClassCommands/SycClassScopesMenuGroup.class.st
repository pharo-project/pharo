"
I am the ""Scopes"" sub-menu of the context menu of classes.
"
Class {
	#name : 'SycClassScopesMenuGroup',
	#superclass : 'ClyMenuGroup',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'accessing' }
SycClassScopesMenuGroup >> iconName [

	^ #smallPushpin
]

{ #category : 'accessing' }
SycClassScopesMenuGroup >> name [

	^ 'Scopes'
]

{ #category : 'accessing' }
SycClassScopesMenuGroup >> order [

	^ 1.5
]

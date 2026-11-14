"
I am the ""Extras"" sub-menu of the context menu of classes.
"
Class {
	#name : 'SycClassExtraMenuGroup',
	#superclass : 'ClyMenuGroup',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'accessing' }
SycClassExtraMenuGroup >> iconName [

	^ #smallRedo
]

{ #category : 'accessing' }
SycClassExtraMenuGroup >> name [

	^ 'Extras'
]

{ #category : 'accessing' }
SycClassExtraMenuGroup >> order [

	^ 10200
]

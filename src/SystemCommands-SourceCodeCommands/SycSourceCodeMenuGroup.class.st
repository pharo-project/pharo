"
I am the ""Source code"" sub-menu of the context menu of code editors. Meta+T opens my commands as a pop-up menu (ClyOpenMenuGroupCommand). Before that, when no text is selected, I show the source node at the cursor, so the commands apply to it.
"
Class {
	#name : 'SycSourceCodeMenuGroup',
	#superclass : 'ClyMenuGroup',
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'accessing' }
SycSourceCodeMenuGroup >> description [

	^ 'Opens special source code menu.'
]

{ #category : 'accessing' }
SycSourceCodeMenuGroup >> iconName [

	^ #smallAuthoringTools
]

{ #category : 'accessing' }
SycSourceCodeMenuGroup >> name [

	^ 'Source code'
]

{ #category : 'accessing' }
SycSourceCodeMenuGroup >> order [

	^ -10
]

{ #category : 'opening' }
SycSourceCodeMenuGroup >> prepareToOpenIn: aToolContext [

	aToolContext selectedTextInterval ifEmpty: [ aToolContext showSourceNode ]
]

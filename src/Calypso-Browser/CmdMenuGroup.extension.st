Extension { #name : #CmdMenuGroup }

{ #category : #'*Calypso-Browser' }
CmdMenuGroup >> buildBrowserNotebookActions [

	self isActive ifFalse: [ ^#()] .
	
	^contents flatCollect: [ :each | each buildBrowserNotebookActions ]
]

{ #category : #'*Calypso-Browser' }
CmdMenuGroup >> buildBrowserTabActions [

	self isActive ifFalse: [ ^#()].
	
	^contents flatCollect: [ :each | each buildBrowserTabActions ]
]

{ #category : #'*Calypso-Browser' }
CmdMenuGroup >> buildBrowserToolbar: toolbarMorph [

	self isActive ifFalse: [ ^self].
	toolbarMorph submorphs ifNotEmpty: [ 
		toolbarMorph addNewItem: ClyToolbarSeparatorMorph new].
	contents do: [ :each | each buildBrowserToolbar: toolbarMorph ]
]

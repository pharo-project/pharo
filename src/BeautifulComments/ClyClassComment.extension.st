Extension { #name : 'ClyClassComment' }

{ #category : '*BeautifulComments' }
ClyClassComment class >> beautifulCommentsCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap tabs add: ClyRichTextClassCommentEditorToolMorph
]

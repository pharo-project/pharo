Extension { #name : 'CmdCommandMenuItem' }

{ #category : '*Calypso-Browser' }
CmdCommandMenuItem >> buildBrowserNotebookActions [

	^ self activator buildBrowserNotebookActions
]

{ #category : '*Calypso-Browser' }
CmdCommandMenuItem >> buildBrowserToolbar: toolbarMorph [

	self activator buildBrowserToolbar: toolbarMorph
]

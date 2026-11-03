Extension { #name : 'CmdMenu' }

{ #category : '*Calypso-Browser' }
CmdMenu >> buildBrowserNotebookActions [

	^ self rootGroup buildBrowserNotebookActions
]

{ #category : '*Calypso-Browser' }
CmdMenu >> buildBrowserToolbar: aToolbar [

	self rootGroup buildBrowserToolbar: aToolbar
]

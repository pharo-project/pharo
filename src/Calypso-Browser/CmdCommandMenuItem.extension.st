Extension { #name : #CmdCommandMenuItem }

{ #category : #'*Calypso-Browser' }
CmdCommandMenuItem >> buildBrowserNotebookActions [
	
	^ activator buildBrowserNotebookActions
]

{ #category : #'*Calypso-Browser' }
CmdCommandMenuItem >> buildBrowserTabActions [
	
	^activator buildBrowserTabActions
]

{ #category : #'*Calypso-Browser' }
CmdCommandMenuItem >> buildBrowserToolbar: toolbarMorph [
	
	activator buildBrowserToolbar: toolbarMorph
]

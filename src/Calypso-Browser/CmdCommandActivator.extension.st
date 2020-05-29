Extension { #name : #CmdCommandActivator }

{ #category : #'*Calypso-Browser' }
CmdCommandActivator >> buildBrowserNotebookActions [

	self canExecuteCommand ifFalse: [ ^ #() ].
	
	^ command buildBrowserNotebookActionsUsing: self
]

{ #category : #'*Calypso-Browser' }
CmdCommandActivator >> buildBrowserTabActions [

	self canExecuteCommand ifFalse: [ ^#() ].
	
	^command buildBrowserTabActionsUsing: self
]

{ #category : #'*Calypso-Browser' }
CmdCommandActivator >> buildBrowserToolbar: toolbarMorph [

	self canExecuteCommand ifFalse: [ ^self ].
	
	command addToolbarItemInto: toolbarMorph using: self
]

{ #category : #'*Calypso-Browser' }
CmdCommandActivator >> decorateTableCell: anItemCellMorph [ 

	command decorateTableCell: anItemCellMorph using: self
]

{ #category : #'*Calypso-Browser' }
CmdCommandActivator >> isCommandAppliedToBrowser [

	^command isAppliedToBrowser
]

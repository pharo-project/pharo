Extension { #name : 'CmdCommandActivator' }

{ #category : '*Calypso-Browser' }
CmdCommandActivator >> buildBrowserNotebookActions [

	self canExecuteCommand ifFalse: [ ^ #(  ) ].

	^ self command buildBrowserNotebookActionsUsing: self
]

{ #category : '*Calypso-Browser' }
CmdCommandActivator >> buildBrowserToolbar: toolbarMorph [

	self canExecuteCommand ifFalse: [ ^ self ].

	self command addToolbarItemInto: toolbarMorph using: self
]

{ #category : '*Calypso-Browser' }
CmdCommandActivator >> decorateTableCell: anItemCellMorph [

	self command decorateTableCell: anItemCellMorph using: self
]

{ #category : '*Calypso-Browser' }
CmdCommandActivator >> isCommandAppliedToBrowser [

	^ self command isAppliedToBrowser
]

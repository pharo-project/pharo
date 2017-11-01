Extension { #name : #MCPatchBrowser }

{ #category : #'*Tool-Diff' }
MCPatchBrowser >> diffSelection [
	"Open a diff browser on the selection."
	
	selection ifNotNil:
		[selection diff]
]

{ #category : #'*Tool-Diff' }
MCPatchBrowser >> methodListMenu: aMenu [

	selection 
		ifNotNil: [ 
			aMenu addList: #(
				('Install'	 installSelection)
				('Diff'	diffSelection)
				('Revert'	 revertSelection)
				-)].
	super methodListMenu: aMenu.
	^ aMenu

]

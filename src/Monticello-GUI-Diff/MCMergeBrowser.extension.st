Extension { #name : 'MCMergeBrowser' }

{ #category : '*Monticello-GUI-Diff' }
MCMergeBrowser >> diffSelection [
	"Open a diff browser on the selection."

	selection ifNotNil:
		[selection diff]
]

{ #category : '*Monticello-GUI-Diff' }
MCMergeBrowser >> methodListMenu: aMenu [

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

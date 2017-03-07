updating
refreshPanes
	self panes do: [ :aPane | 
		aPane updateToolbar.
		aPane presentations presentations first withAllPresentations do: [ :aPresentation |
			aPresentation wantsAutomaticRefresh ifTrue: [ 
				aPresentation requestRefresh ] ]  ]
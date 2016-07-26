initialization
initializePresenter

	packages whenSelectedItemChanged: [ :item |	self updateRemoteSelectionFrom: item ].
		
	packages whenRootsChanged: [ :nodes | 
		nodes ifNotEmpty: [ nodes first takeHighlight ] ].
	
	remotes selectedChangedBlock: [:remote :selected | 
		self doIfNotSilent: [ self change: selected forRemote: remote ]].
	
	self whenBuiltDo: [ | nodes |
		nodes := packages roots.
		nodes ifNotEmpty: [
			packages selectedItem: (
			nodes first 
				selected: true;
				takeHighlight;
				yourself ) ] ]
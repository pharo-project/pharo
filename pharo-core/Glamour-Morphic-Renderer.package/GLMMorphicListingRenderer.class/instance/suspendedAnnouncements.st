private
suspendedAnnouncements
	^ OrderedCollection  
		with: GLMTreeMorphSelectionChanged 
		with: GLMTreeMorphStrongSelectionChanged 
		with: GLMMenuItemSelected 
		with: GLMKeyStroke
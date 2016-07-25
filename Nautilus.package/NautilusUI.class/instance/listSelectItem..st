private
listSelectItem: anObject 
	| selection |
	
	selection := anObject asNautilusSelection.
	self packageWidget  
		expandAllFromNode: selection nodePath first;
		selectItems: { selection item }.
	self packageWidget scrollSelectionIntoView.
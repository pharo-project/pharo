package filter
updatePackages
	| package |
	package := self selectedPackage.
	package 
		ifNotNil: [ 
			self packageWidget 
					updateContentsWithPreviouslyExpanded: package item asNautilusItemPath;
					scrollSelectionIntoView ]
		ifNil: [ self packageWidget updateContentsWithPreviouslyExpanded: #() ]
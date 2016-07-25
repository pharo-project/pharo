instance creation
selectItems: itemsList
	^ self new action: #selectItems:; arguments: (Array with: itemsList)
private
change: selected forRemote: remote
	| selectedPackage group |
	
	selectedPackage := packages selectedItem.
	selectedPackage ifNil: [ ^ self ].
	
	group := selectedPackage content repositoryGroup.
	
	selected
		ifTrue: [ group addRepository: remote ]
		ifFalse: [ group removeRepository: remote ]
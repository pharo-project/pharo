instance creation
breadthFirst: aReference
	^ self breadthFirst: aReference collect: [:entry | entry]
instance creation
preorder: aReference
	^ self preorder: aReference select: [:entry | true]
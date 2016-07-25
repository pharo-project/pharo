instance creation
postorder: aReference
	^ self postorder: aReference select: [:entry | true]
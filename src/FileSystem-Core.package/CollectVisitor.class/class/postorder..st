instance creation
postorder: aReference
	^ self postorder: aReference collect: [:entry | entry]
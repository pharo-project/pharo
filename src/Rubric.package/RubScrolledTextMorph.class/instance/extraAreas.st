accessing rulers
extraAreas
	^ self rulers select: [ :r | r isOver ]

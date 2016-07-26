private
checkSize
	
	[ storage size >= self maxSize ]
		whileTrue: [ 
			storage removeFirst.
			index := index -1].
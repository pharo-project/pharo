accessing
totalSize
	^ resources 
		ifNil: [ 0 ] 
		ifNotNil: [ 
			(resources values collect: [ :pool | 
				pool
					inject: 0
					into: [ :sum :each | 
						sum + (each ifNil: [ 0 ] ifNotNil: [ each size ]) ] ]) sum ]
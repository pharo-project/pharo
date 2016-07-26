public
maximumSize: anIntegerOrNil

	maximumSize := anIntegerOrNil.
	maximumSize ifNotNil:[
		used > maximumSize
			ifTrue:["shrink"
				self shrinkTo: maximumSize]]
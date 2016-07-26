testing
canProcess
	[ self value ]
		on: GTExampleError
		do: [ ^ false ].
	^ true
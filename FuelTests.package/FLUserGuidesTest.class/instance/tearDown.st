running
tearDown
	super tearDown.
	#('demo.fuel' 'numbers.fuel' 'number.fuel.zip' 'example.FL' ) 
		do: [:each | self deleteFileNamed: each ].
		
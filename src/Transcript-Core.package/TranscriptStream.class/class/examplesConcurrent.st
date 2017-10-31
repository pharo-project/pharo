examples
examplesConcurrent
	"self examplesConcurrent"

	| transcript |
	
	transcript := self new.
	transcript open.
	
	[ 	
		1 to: 10 do: [ :i | 
			transcript
				nextPutAll: i printString;
				nextPutAll: '*'.
			Processor yield 
		].
		transcript flush 
		
	] fork.

	[ 	
		100 to: 110 do: [ :i | 
			transcript
				nextPutAll: i printString;
				nextPutAll: '-'.
			Processor yield 
		].
		transcript flush 
		
	] fork
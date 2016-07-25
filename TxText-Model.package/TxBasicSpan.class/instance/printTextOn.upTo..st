printing
printTextOn: aStream upTo: endSpan 
	self do: [ :eachSpan |
		
		eachSpan == endSpan ifTrue: [ ^self ].
		eachSpan asStringOn: aStream.			
		
	 ]

		
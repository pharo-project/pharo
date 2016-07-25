running
testJoinUsingLastOnArray
	
	{	{  1. 2. 3 } 	-> '1, 2 and 3' .
		{  1. 2 } 	-> '1 and 2' .
		{  1 } 		-> '1' .
		{  }			-> ''.
	} 	asDictionary keysAndValuesDo: 
		[ 	:testCollection :resultString |
			self	 assert: ( testCollection joinUsing: ', ' last: ' and ') equals: resultString. 
		]

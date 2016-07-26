accessing
runs: anArray
	anArray size = string size
		ifFalse:
			[^self error: 'Some code is setting text attributes length not matching the string size'].
	runs := anArray
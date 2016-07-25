private
substrings: aString do: aBlock
	(aString substrings: ' .,;') do: [ :each | 
		aBlock value: each trimBoth ]
conversion
asString 
		
	^String streamContents: [ :s | 
		self normalized start printTextOn: s upTo: self normalized end.		
	]
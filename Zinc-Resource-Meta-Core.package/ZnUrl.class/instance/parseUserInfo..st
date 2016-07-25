parsing
parseUserInfo: stream
	| userString |
	(userString := stream upTo: $:) isEmpty 
		ifFalse: [  
			self username: (self decodePercent: userString) ].
	stream atEnd 
		ifFalse: [ 
			self password: (self decodePercent: stream upToEnd) ]
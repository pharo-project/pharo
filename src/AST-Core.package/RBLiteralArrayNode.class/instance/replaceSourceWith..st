private-replacing
replaceSourceWith: aNode 
	(self class = aNode class and: 
			[self isForByteArray = aNode isForByteArray 
				and: [self contents size = aNode contents size]]) 
		ifFalse: [^super replaceSourceWith: aNode].
	self contents
		with: aNode contents 
		do: [:first :second | first = second ifFalse: [ first replaceSourceWith: second]]
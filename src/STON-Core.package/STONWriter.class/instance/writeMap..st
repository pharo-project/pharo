writing
writeMap: hashedCollection
	self with: hashedCollection do: [ 
		self encodeMap: hashedCollection ]
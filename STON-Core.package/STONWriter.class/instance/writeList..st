writing
writeList: collection
	self with: collection do: [ 
		self encodeList: collection ]
stream out
next: count putAll: collection startingAt: offset
	"Write count bytes from collection starting at offset.
	This is an inefficient abstract implementation writing bytes one by one."
	
	0 to: count - 1 do: [ :each | 
		self nextPut: (collection at: offset + each) ]
enumerating
allPairsDo: aBinaryBlock
	"
	Iterate over all the pairs of the receiver and apply the binaryBlock
	to each pair. 
	
	| res | 
	res := OrderedCollection new. 
	#(0 1 2 3) allPairsDo: [:first :second | res add: {first . second} ].
	res 
	 an OrderedCollection(#(0 0) #(0 1) #(0 2) #(0 3) #(1 0) #(1 1) #(1 2) #(1 3) #(2 0) #(2 1) #(2 2) #(2 3) #(3 0) #(3 1) #(3 2) #(3 3))
	"
	
	self do: [:first| self do: [:second| aBinaryBlock value: first value: second]]
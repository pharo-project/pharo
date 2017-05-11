enumerating
groupsOf: n atATimeCollect: aBlock 
	"Evaluate aBlock with my elements taken n at a time. Ignore any leftovers at the end. 
	Allows use of a flattened array for things that naturally group into groups of n. 
	If aBlock has a single argument, pass it an array of n items, otherwise, pass the items as separate arguments. See also pairsDo:"
	
	"
	#(16 17 17 16 18 17 18   19 19 19 18 19 19 20   19 20 19 20 20 20 19   20) groupsOf: 7 atATimeCollect: [ :x | x ]
	>>> #(#(16 17 17 16 18 17 18) #(19 19 19 18 19 19 20) #(19 20 19 20 20 20 19))
	"
	"
	#(1 1 1 10 10 10 100  100 100) 
		groupsOf: 3 atATimeCollect: [ :x :y :z | x + y + z] 
	>>> #(3 30 300)
	"
	
	"
	#(1 1 1 10 10 10 100  100 100) 
		groupsOf: 3 atATimeCollect: [ :x | x ]  
	>>> #(#(1 1 1) #(10 10 10) #(100 100 100))
	"
	
	| passArray |
	passArray := aBlock numArgs = 1.
	^(n
		to: self size
		by: n)
		collect: [:index | | args | 
			args := (self copyFrom: index - n + 1 to: index) asArray.
			passArray
				ifTrue: [aBlock value: args]
				ifFalse: [aBlock valueWithArguments: args]]
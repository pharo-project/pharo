method-compiling
methodsForBehavior: aBehavior
	"Get all the methods we consider we want from the current environment and return a copy, binded to the new class"
	^ aBehavior methods 
		reject: [ :m | (self shouldMethodBeRemoved: m) or: [m selector = #trace:on:] ]
		thenCollect: [ :m | self createNewMethod: m ].
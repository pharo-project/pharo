*GT-Debugger-Model
debuggersMatching: aBlock

	^ (self debuggersWithRanksMatching: aBlock) 
			inject: OrderedCollection new
			into: [ :aCollection :aPair | aCollection add: aPair key; yourself ]
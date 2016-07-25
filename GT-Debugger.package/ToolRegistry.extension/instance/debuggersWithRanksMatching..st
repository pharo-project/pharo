*GT-Debugger-Model
debuggersWithRanksMatching: aBlock

	^ ((self registeredDebuggersWithRanks) 
			select: [ :aPair | aBlock value: aPair key ])
			sort: [ :pair1 :pair2 | pair1 value > pair2 value ].
private
lookForMoreMatchesInContext: oldContext 
	oldContext keysAndValuesDo: [:key :value | 
		(key isString not and: [key recurseInto]) 
			ifTrue: 
				[oldContext at: key put: (value collect: [:each | self visitNode: each])]]
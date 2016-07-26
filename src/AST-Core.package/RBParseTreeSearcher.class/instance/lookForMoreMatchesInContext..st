private
lookForMoreMatchesInContext: oldContext 
	oldContext keysAndValuesDo: 
			[:key :value | 
			(key isString not and: [key recurseInto]) 
				ifTrue: [value do: [:each | self visitNode: each]]]
accessing
instanceMethodChanges

	^ classRemoved methods 
		collect: [ :aCompiledMethod | 
			EpMethodRemoval method: aCompiledMethod ]
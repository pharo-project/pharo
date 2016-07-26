accessing
classMethodChanges

	^ classRemoved theMetaClass methods 
		collect: [ :aCompiledMethod | 
			EpMethodRemoval method: aCompiledMethod ]
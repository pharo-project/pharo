*Pharo30Bootstrap
recordVersion: aMetacelloVersion group: aGroup
	
	[ ^(aMetacelloVersion ignoreImage: true) record: aGroup ] 
		on: MCMergeOrLoadWarning do: [ :e | "we merge" e resume: false ]
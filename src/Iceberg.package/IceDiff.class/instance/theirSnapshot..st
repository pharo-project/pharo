querying
theirSnapshot: package 
	^ theirVersion ifNotNil: [ 
		theirVersion snapshotFor: package ]
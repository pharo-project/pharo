querying
snapshotFor: package 
	^ (self versionFor: package) ifNotNil: #snapshot
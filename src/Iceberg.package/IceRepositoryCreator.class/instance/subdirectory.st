accessing
subdirectory
	^ subdirectory ifNil: [ self class defaultSubdirectory ]
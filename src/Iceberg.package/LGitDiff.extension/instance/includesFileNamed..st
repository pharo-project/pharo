*Iceberg-Core
includesFileNamed: path
	^ (self filesDo: [ :delta :second :third | 
		| modifiedFileName |
		modifiedFileName := delta newFile path.
		(modifiedFileName beginsWith: path) asAlien
	]) asBoolean.

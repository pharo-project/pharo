public - string writing
deferFlushDuring: aBlock

	"defer flusing the filestream until a block has been executed"
	
	flushChanges ifFalse: [ ^ aBlock value ].
	
	flushChanges := false.
	
	^ [ aBlock value ] ensure: [ flushChanges := true . self flushChangesFile ]
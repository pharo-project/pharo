execute
ensureChangeLog
	^ self hasChangeLog 
		ifFalse: [ self defineChangeLogTemplate ]
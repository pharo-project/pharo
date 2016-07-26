testing
isEmpty
	"Answer whether the receiver contains any elements."
	
	changeRecords ifNil: [^true].
	^ changeRecords isEmpty 
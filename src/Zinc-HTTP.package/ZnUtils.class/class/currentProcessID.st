accessing
currentProcessID
	"Return a integer of at most 6 digits that can be used as a description for the current active process.
	This is a reduced hash that is not necessarily unique (but uniqueness is very probably)"
	
	^ Processor activeProcess hash \\ 999999
special objects
specialNargsAt: anInteger 
	"Answer the number of arguments for the special selector at: anInteger."

	^ (self specialObjectsArray at: 24) at: anInteger * 2
printing
debugStack: stackSize on: aStream
	"print a condensed version of the stack up to stackSize on aStream"

	(self stackOfSize: stackSize)
		do: [ :item | 
			item printDebugOn: aStream.
			aStream cr ]
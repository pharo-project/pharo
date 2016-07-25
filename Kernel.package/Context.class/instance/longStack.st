debugger access
longStack
	"Answer a String showing the top 100 contexts on my sender chain."

	^ String streamContents: [ :stream |
		(self stackOfSize: 100) do: [ :item | 
			stream print: item; cr ]]
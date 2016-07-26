debugger access
shortStack
	"Answer a String showing the top ten contexts on my sender chain."

	^ String streamContents: [ :stream |
		(self stackOfSize: 10) do: [ :item |
			stream print: item; cr]]
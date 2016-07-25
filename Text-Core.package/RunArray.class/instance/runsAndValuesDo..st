enumerating
runsAndValuesDo: aBlock
	"Evaluate aBlock with run lengths and values from the receiver"
	^runs with: values do: aBlock.
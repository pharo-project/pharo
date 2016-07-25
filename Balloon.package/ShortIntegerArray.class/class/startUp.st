system startup
startUp
	"Check if the word order has changed from the last save"
	((LastSaveOrder at: 1) = 42 and:[(LastSaveOrder at: 2) = 13]) 
		ifTrue:[^self]. "Okay"
	((LastSaveOrder at: 2) = 42 and:[(LastSaveOrder at: 1) = 13]) 
		ifTrue:[^self swapShortObjects]. "Reverse guys"
	^self error:'This must never happen'
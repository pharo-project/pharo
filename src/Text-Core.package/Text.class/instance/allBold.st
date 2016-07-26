emphasis
allBold 
	"Force this whole text to be bold."
	string size = 0 ifTrue: [^self].
	self makeBoldFrom: 1 to: string size
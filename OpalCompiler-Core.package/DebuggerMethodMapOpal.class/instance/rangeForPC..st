public
rangeForPC: aPC
	"return the debug highlight for aPC"
	
	^self rangeForPC: aPC contextIsActiveContext: false
	
operations
controlsExtent
	"Answer the min extent of the controls."

	^self controls first minExtent + (self controls last minExtent x @ 0)
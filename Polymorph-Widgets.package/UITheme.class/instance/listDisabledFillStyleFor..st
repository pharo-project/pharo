fill-styles
listDisabledFillStyleFor: aList
	"Return the disabled fillStyle for the given list."
	
	^aList paneColor alphaMixed: 0.3 with: Color white
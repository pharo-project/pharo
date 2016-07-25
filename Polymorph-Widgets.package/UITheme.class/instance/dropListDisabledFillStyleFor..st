fill-styles
dropListDisabledFillStyleFor: aDropList
	"Return the disabled fillStyle for the given drop list."
	
	^aDropList paneColor alphaMixed: 0.3 with: Color white
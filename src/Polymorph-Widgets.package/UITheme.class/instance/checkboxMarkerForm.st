label-styles
checkboxMarkerForm
	"Answer the form to use for a checkbox marker."

	^self forms at: #checkboxMarker ifAbsent: [Form extent: 12@12 depth: Display depth]
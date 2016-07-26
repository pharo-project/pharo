label-styles
radioButtonMarkerForm
	"Answer the form to use for a radio button marker."

	^self forms at: #radioButtonMarker ifAbsent: [Form extent: 12@12 depth: Display depth]
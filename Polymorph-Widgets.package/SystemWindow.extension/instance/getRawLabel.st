*Polymorph-Widgets
getRawLabel
	"Answer a shallowCopy of the label with the contents fitted."
	
	|contentsFit|
	contentsFit := label shallowCopy fitContents.
	contentsFit extent: (label extent x min: contentsFit extent x) @ contentsFit extent y.
	^contentsFit
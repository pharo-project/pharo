new selection
selectInvisiblyFrom: start to: stop
	"Select the designated characters, inclusive.  Make no visual changes."

	self markIndex: start; pointIndex: stop + 1
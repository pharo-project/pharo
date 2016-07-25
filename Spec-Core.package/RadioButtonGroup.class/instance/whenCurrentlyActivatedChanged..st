api-events
whenCurrentlyActivatedChanged: aBlock
	"This method is triggered when `canDeselectByClick` changes"
	
	currentlyActivated whenChangedDo: aBlock
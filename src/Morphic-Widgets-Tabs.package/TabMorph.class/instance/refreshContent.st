private
refreshContent
	
	morph := nil.
	self triggerEvent: #tabEmptyContents with: self.
	
	"Trigger the update"
	self softRetrieveMorph.
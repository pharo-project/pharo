initialization
newButtons
	"Answer new buttons as appropriate."
	self filterMorph: self newFilterEntry.
	self okButton: self newOKButton.
	^Array 
		with: self filterMorph
		with: self okButton
		with: self newCancelButton
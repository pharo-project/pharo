private
strongSelectionChanged: ann
	self doubleClickSelector ifNil: [ ^ self ].
	
	self model perform: self doubleClickSelector withEnoughArguments: { ann }
iterating
nodesDo: aBlock 
	aBlock value: self.
	self children do: [ :each | each nodesDo: aBlock ]
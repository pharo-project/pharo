walking
commitsDo: aBlock 
	self rawResultsDo: [ :result | aBlock value: (self commitFrom: result) ]
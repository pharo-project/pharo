enumerating - assoctiations
associationsDo: aBlock 
	self keysAndValuesDo: [:key :value | aBlock value: key -> value]
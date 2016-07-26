accessing
classificationTable
	classificationTable 
		ifNil: [ self initializeClassificationTable ].
	^ classificationTable
running
allExamplesClasses
	" IMPORTANT NOTICE 
	1) We expicitly exclude examples from Roassal and Woden. Running all of those examples at once will severely slow down all tests and eventually cause segmentation faults - crash of image. This is not a fault/bug of GTExample - it happens also if the GTExample-wrapper is not used at all ! "
	^ ((Object withAllSubclasses 
		reject: [ :each | 
			(each name beginsWith: 'RT') 
				or: [ (each name beginsWith: 'RW')
					or: [ each name beginsWith: 'GTDummy' ] ] ]) 
		sort: [ :a :b | a name <= b name ])
initialize
initialize

	super initialize.
	
	images := { self class imageData1. self class imageData2. self class imageData3 } collect: [ :data |
		self formFromContents: data ]. 
	currentImageIndex := 1.
	self extent: 16@16.
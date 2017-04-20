instance creation
newDataSourceMatching: aFTFilter
	^ (super newDataSourceMatching: aFTFilter)
		glamourPresentation: self glamourPresentation;
		yourself
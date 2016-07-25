as yet unclassified
updateSelectedLocation
	"Position the location morph to indicate the selected colour."
	
	self locationMorph
		position: (self selectedLocation - (self locationMorph extent // 2 + (self locationMorph extent \\ 2)))
protocol
updateLabel
	"Update the label."
	
	self model ifNotNil: [:m |
		self getLabelSelector ifNotNil: [:selector | 
			self label: (m perform: selector) ]]
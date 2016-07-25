initialize
menuImage
	"answer a form to be used in the menu button"
	^ self class
		boxOfSize: (self buttonExtent x min: self buttonExtent y)
		color: self thumbColor
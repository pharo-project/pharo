initialize
downImage
	"answer a form to be used in the down button"
	^ self class
		arrowOfDirection: (bounds isWide
				ifTrue: [#right]
				ifFalse: [#bottom])
		size: (self buttonExtent x min: self buttonExtent y)
		color: self buttonImageColor
initialize
upImage
	"answer a form to be used in the up button"
	^ self class
		arrowOfDirection: (bounds isWide
				ifTrue: [#left]
				ifFalse: [#top])
		size: (self buttonExtent x min: self buttonExtent y)
		color: self buttonImageColor
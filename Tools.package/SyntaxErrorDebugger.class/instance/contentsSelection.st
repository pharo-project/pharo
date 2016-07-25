other
contentsSelection
	"Return the interval of text in the code pane to select when I set the pane's contents"

	^ location 
		ifNil: [1 to: 0]  "null selection"
		ifNotNil: [ location to: location + self sourceErrorString size - 1 ] 
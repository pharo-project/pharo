accessing - margin/padding
padding

	"top right bottom left"
	^ padding ifNil: [ padding := Array braceWith: 0 with: 0 with: 0 with: 0 ]

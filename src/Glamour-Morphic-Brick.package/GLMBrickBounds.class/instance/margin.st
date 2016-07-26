accessing - margin/padding
margin

	"top right bottom left"
	^ margin ifNil: [ margin := Array braceWith: 0 with: 0 with: 0 with: 0 ]

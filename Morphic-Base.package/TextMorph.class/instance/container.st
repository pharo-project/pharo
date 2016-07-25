geometry
container
	"Return the container for composing this text.  There are four cases:
	1.  container is specified as, eg, an arbitrary shape,
	2.  container is specified as the bound rectangle, because
		this morph is linked to others,
	3.  container is nil, and wrap is true -- grow downward as necessary,
	4.  container is nil, and wrap is false -- grow in 2D as nexessary."

	container ifNil:
		[successor ifNotNil: [^ self compositionRectangle].
		wrapFlag ifTrue: [^ self compositionRectangle withHeight: 9999999].
		^ self compositionRectangle topLeft extent: 9999999@9999999].
	^ container